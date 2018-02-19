import System.Environment (getArgs, getProgName)
import System.FilePath (addExtension, dropExtension)
import Lex
import Synt
import HeaderGen
import Format

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (inputFileName:_) -> do
            contents <- readFile inputFileName
            let tree = synt $ alexScanTokens contents
            let headerCTokens = createHeader tree
            writeFiles inputFileName $ format headerCTokens

    where
        writeFiles inputFileName headerContent = do
            let baseFileName = (dropExtension inputFileName) ++ "_parser"
            let parserHeaderFileName = addExtension baseFileName "h"
            writeFile parserHeaderFileName headerContent

        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
