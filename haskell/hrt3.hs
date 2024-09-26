import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
    n <- readFile "/root/devops/logs/tail.conf"
    let numLines = read n :: Int
    logContents <- BS.readFile "/root/devops/logs/file.log"
    let allLines = reverse $ BC.lines logContents
    let lastNLines = take numLines allLines
    let result = reverse lastNLines
    mapM_ BC.putStrLn result
