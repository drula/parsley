import System.Environment (getArgs, getProgName)
import System.FilePath (addExtension, dropExtension)
import Lex
import Synt
import HeaderGen

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (inputFileName:_) -> do
            contents <- readFile inputFileName
            let tree = synt $ alexScanTokens contents
            let header = createHeader tree
            writeFiles inputFileName header
    where
        writeFiles inputFileName header = do
            let baseFileName = (dropExtension inputFileName) ++ "_parser"
            let parserHeaderFileName = addExtension baseFileName "h"
            writeFile parserHeaderFileName $ show header

        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
