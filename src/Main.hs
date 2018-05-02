import System.Environment (getArgs, getProgName)
import Lex
import Synt
import Translator
import FileWriter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (inputFileName:_) -> do
            contents <- readFile inputFileName
            let tree = synt $ alexScanTokens contents
            let result = translate tree

            writeHeader inputFileName result
            writeSource inputFileName result
    where
        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
