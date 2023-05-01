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
    
    let partCount = convertToInt16 $ BS.take 2 $ BS.drop 0xF brokenContent
    print $ "Number of parts: " ++ (show partCount)
    
    
    let faceData = chunksOf 3 $ Prelude.reverse $ Prelude.take (partCount * 3) $ Prelude.reverse $ tokenise (BSU.pack "\NUL\NUL") $ BS.dropEnd 2 brokenContent
    printFaceData faceData
    print "end"


printFaceData faceData = do
    let selectedData = Prelude.head faceData
    let vertices = Prelude.head selectedData
    let colors = Prelude.last selectedData
    
    Prelude.putStrLn "\n\nvertices: "
    print vertices
    
    Prelude.putStrLn "colors: " 
    print colors
    
    if Prelude.length faceData > 1
        then printFaceData $ Prelude.tail faceData
        else print "test"


convertToInt16 :: ByteString -> Int
convertToInt16 bytes = (fromIntegral((Maybe.fromJust(bytes!?0))) * 256 + fromIntegral(Maybe.fromJust(bytes!? 1)))


tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = breakSubstring x y
