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
            let headerContent = createHeader tree
            writeFiles inputFileName headerContent

    where
        writeFiles inputFileName headerContent = do
            let baseFileName = (dropExtension inputFileName) ++ "_parser"
            let parserHeaderFileName = addExtension baseFileName "h"
            let parserSourceFileName = addExtension baseFileName "c"
            writeFile parserHeaderFileName headerContent
            writeFile parserSourceFileName "parser source\n"

        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
