{- Haskell implemention of pack, the early 80s file compression tool still
 - supported by gzip.
 - By Vidar 'koala_man' Holen, 2017
 -}

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Word
import Data.List
import Data.Bits
import GHC.IO.Handle
import System.Environment
import System.IO
import System.Exit
import Debug.Trace

data Node = Branch Node Node | Leaf Word16
    deriving (Show, Eq)

-- Maximum number of bits we can use
maxBits = 25

-- special EOF value
eof = 256

countOccurrences :: B.ByteString -> M.Map Word8 Word32
countOccurrences = B.foldl' (flip bump) M.empty
  where
    bump key = M.insertWith (const (+1)) key 1

-- Naive implementation of Package-Merge to find a Huffman
-- encoding with max tree depth constraints.
packageMerge :: M.Map Word8 Word32 -> [[Word16]]
packageMerge counts = flipLevels . countLeaves $ iterate maxBits []
  where
    getList :: M.Map Word8 Word32 -> [([Word16], Word64)]
    getList m = ([eof], 1) :
        if M.null m
        then [([0x00], 2)] -- Ensure a minimum bitlength of 1
        else (map (\(k,v) -> ([fromIntegral k], 2*(fromIntegral v))) $ M.toList m)
    list = getList counts
    sortList = sortOn snd
    reduce l =
        case l of
            (t1, v1):(t2, v2):rest -> (t1 ++ t2, v1+v2) : reduce rest
            [_] -> []
            [] -> []
    iterate 0 l = l
    iterate n l =
        let next = reduce $ sortList (l ++ list)
        in if l == next then l else iterate (n-1) next
    countLeaves :: [([Word16], Word64)] -> [(Word16, Int)]
    countLeaves = map (\l -> (head l, length l)) . group . sort . concatMap fst

    flipLevels :: [(Word16, Int)] -> [[Word16]]
    flipLevels pairs = M.elems $ foldl' bump (M.fromList $ map (\x->(x,[])) [0..maxBits]) pairs
    bump map (w16,level) = M.insertWith (++) level [w16] map

-- Take a list of leaf nodes per level and build a Huffman tree
-- where the longest path is leftmost.
growTree :: [[Word16]] -> Node
growTree levels =
    case mk (map sort $ levels) of
        [single] -> single
        rest -> error $ (show levels) ++ ", " ++ (show rest)
  where
    mk [last] = map Leaf last
    mk (level:rest) = collect (mk rest) ++ mk [level]
    collect [] = []
    collect (a:b:rest) = Branch a b : collect rest

-- Create a map from Word16 to 1-bit-per-byte ByteStrings
makeEncoding :: Node -> M.Map Word16 B.ByteString
makeEncoding node = M.fromList $ mk node []
  where
    mk node prefix =
        case node of
            Leaf n -> [(n, B.pack $ reverse prefix)]
            Branch l r -> mk l (0:prefix) ++ mk r (1:prefix)

-- Check whether we will trip gzip bug #28861
encodingTriggers28861 :: M.Map Word16 B.ByteString -> Bool
encodingTriggers28861 map = maximum (M.elems map) /= M.findWithDefault (error "Missing eof") eof map

-- Encode a ByteString as a ByteString of 1s and 0s.
encodeBits :: M.Map Word16 B.ByteString -> B.ByteString -> B.ByteString
encodeBits mp string =
    (B.concatMap (lookup . fromIntegral) string) `B.append` (lookup eof)
  where
    lookup key = M.findWithDefault (error $ "Unknown item: " ++ (show key)) key mp

-- Pack bits as bytes
packBits :: Monad m => B.ByteString -> (Word8 -> m ()) -> m ()
packBits str f = do
    let byte = M.lookupGE first lookupTable
    case byte of
        Nothing -> error $ "Unknown byte: " ++ (show first)
        Just (k,v) -> f v
    unless (B.null rest) $
        packBits rest f
  where
    (first, rest) = B.splitAt 8 str
    lookupTable = M.fromList $ generate 8 [] 0
    generate n prefix w =
        if n == 0
        then [(B.pack $ reverse prefix,w)]
        else generate (n-1) (0:prefix) (w*2) ++ generate (n-1) (1:prefix) (w*2+1)

packBitsToHandle :: B.ByteString -> Handle -> IO ()
packBitsToHandle str output = packBits str (B.hPut output . B.singleton)

-- Network byte order conversion
w32ToByteString :: Word32 -> B.ByteString
w32ToByteString value =
    B.pack $ map (\i -> fromIntegral $ (value `shiftR` (i*8)) .&. 0xFF) [3,2,1,0]

-- Return two IO actions for reading input from a handle.
-- The first must be used and completely closed before using the other.
duplicateInput :: Handle -> IO (IO B.ByteString, IO B.ByteString)
duplicateInput input = do
    seekable <- hIsSeekable input
    if seekable
      then do
        pos <- hTell input
        dupe <- hDuplicate input
        return (B.hGetContents input,
                hSeek dupe AbsoluteSeek pos >> B.hGetContents dupe)
      else do
        str <- B.hGetContents input
        return (return str, return str)

-- Get leaf nodes at each level of the tree
getLevels :: Node -> [[Word16]]
getLevels tree = f [tree]
  where
    f [] = []
    f list = (map fromLeaf $ filter isLeaf list) : f (concatMap successor list)
    isLeaf x = case x of Leaf _ -> True; _ -> False
    successor (Leaf _) = []
    successor (Branch x y) = [x,y]

encodeLeafCountPerLevel :: [[Word16]] -> [Word8]
encodeLeafCountPerLevel l =
    case l of
        -- The last level necessarily has at least 2 nodes,
        -- but may have as many as 257 (Word8 + eof), so
        -- 'pack' encodes it minus 2.
        [last] -> [fromIntegral $ (length last) - 2]
        (first:rest) -> (fromIntegral $ length first) : encodeLeafCountPerLevel rest
        [] -> error "Missing tree levels"

fromLeaf (Leaf x) = x

pack input output = do
    hSetBinaryMode input True
    (pass1, pass2) <- duplicateInput input
    counts <- countOccurrences <$> pass1
    let tree = growTree $ packageMerge counts

    let size = sum $ M.elems counts
    tree `seq` size `seq` return ()

    let encoding = makeEncoding tree
    let levels = drop 1 $ getLevels tree

    when (encodingTriggers28861 encoding) $ do
        hPutStrLn stderr "Warning: this file will trigger gzip issue #28861 and fail to unpack"
        hPutStrLn stderr "         with gzip version 1.6 through 1.8 (16977ae7 -- 79f88bd1)."

    -- Header
    B.hPut output $ B.pack [0x1F, 0x1E]

    -- File length
    B.hPut output $ w32ToByteString size

    -- Max bit length
    bPut $ length levels

    -- Leaf count at each bit length
    B.hPut output . B.pack $ encodeLeafCountPerLevel levels

    -- One byte per leaf
    mapM_ (bPut . fromIntegral) $
        filter (/= eof) $ concat levels

    bits <- encodeBits encoding <$> pass2
    packBitsToHandle bits output

  where
    bPut = B.hPut output . B.singleton . fromIntegral


main = do
    outputTTY <- hIsTerminalDevice stdout

    if outputTTY
        then do
            hPutStrLn stderr "Usage: pack < inputfile  > outputfile"
            hPutStrLn stderr "       (stdout is currently a tty)"
            exitFailure
        else pack stdin stdout
