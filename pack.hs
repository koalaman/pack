import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Array.ST
import Data.Word
import Data.List
import Data.Bits
import GHC.IO.Handle
import System.Environment
import System.IO
import System.Exit
import Debug.Trace

data Node = Branch Node Node | Leaf Word16
    deriving (Show)

eof = 256

countOccurrences :: B.ByteString -> M.Map Word8 Word32
countOccurrences bs = B.foldl' (flip bump) M.empty bs
  where
    bump key map = M.insertWith (const (+1)) key 1 map

-- Create a map from Count to Leaf nodes, with Char as disambiguator.
invertCount :: M.Map Word8 Word32 -> M.Map (Word32,Word16) Node
invertCount = M.fromList . map (\(k,v) -> ((v, fromIntegral k),Leaf (fromIntegral k))) . M.toList

-- pack explicitly encodes eof as a character
insertEOF = M.insert (1, eof) (Leaf eof)

-- Repeatedly join the two smallest counts until we have a single Node.
assembleTree :: M.Map (Word32,Word16) Node -> Node
assembleTree mp =
    case M.toList smallest of
        [((count1,key1),node1), ((count2,key2),node2)] ->
            assembleTree $ M.insert (count1+count2, key1) (Branch node1 node2) rest
        [(_, node)] -> node
        [] -> Leaf 0
  where (smallest, rest) = M.splitAt 2 mp

-- Build a Huffman tree
buildTree :: M.Map Word8 Word32 -> Node
buildTree = assembleTree . insertEOF . invertCount . M.map (*2)

-- Make tree more compatible by guaranteeing 1 bit per symbol.
tweakTree :: Node -> Node
tweakTree t =
    case t of
        Branch {} -> t
        Leaf w -> Branch t (Leaf 0)

-- Align a tree so that left side is deepest,
-- with eof right-most at the deepest level.
alignTree :: Node -> Node
alignTree tree = case mk levels of [single] -> single
  where
    levels = map sort $ getLevels tree
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
    let size = sum $ M.elems counts
    let tree = alignTree . tweakTree . buildTree $ counts
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
