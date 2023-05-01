import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSU
import Data.Maybe as Maybe
import Data.String as String
import Data.List.Split
import GHC.Word
import System.Environment


main = do
    args <- getArgs
    content <- BS.readFile $ args!!0
    let brokenContent = BS.reverse $ fst $ BS.breakSubstring (BSU.pack "    ") (BS.reverse content)
    
    let partCount = convertTo_uI16 $ BS.unpack $ BS.take 2 $ BS.drop 0xF brokenContent
    print $ "Number of parts: " ++ (show partCount)
    
    
    let faceData = chunksOf 3 $ Prelude.reverse $ Prelude.take (partCount * 3) $ Prelude.reverse $ tokenise (BSU.pack "\NUL\NUL") $ BS.dropEnd 2 brokenContent
    printFaceData faceData
    print "end"


printFaceData faceData = do
    let selectedData = Prelude.head faceData
    let vertices = Prelude.head selectedData
    let colors = Prelude.last selectedData
    
    --print $ Prelude.length selectedData
    Prelude.putStrLn "\n\nvertices: "
    print $ formatVerts vertices
    
    Prelude.putStrLn "colors: " 
    print colors
    
    if Prelude.length faceData > 1
        then printFaceData $ Prelude.tail faceData
        else print "End"


convertTo_uI16 :: [Word8] -> Int
convertTo_uI16 bytes | Prelude.length bytes == 2 = (fromIntegral(bytes!!0) * 256 + fromIntegral(bytes!!1))
                     | Prelude.length bytes /= 2 = 0


convertTo_sI16 :: [Word8] -> Int
convertTo_sI16 bytes | fromIntegral(bytes!!0) < 128  = convertTo_uI16 bytes
                     | fromIntegral(bytes!!0) >= 128 = (convertTo_uI16 bytes) - 65536

formatVerts :: ByteString -> [[Int]]
formatVerts bytes = chunksOf 3 $ Prelude.map convertTo_sI16 $ chunksOf 2 $ BS.unpack bytes

tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = breakSubstring x y
