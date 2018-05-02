import System.Environment (getArgs, getProgName)
import Lex
import Synt
import CTypes
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
            let (result @ (CContent typedefs functions)) = translate tree

            writeHeader inputFileName result
            writeSource inputFileName result

            putStrLn $ "Typedefs:\n" ++ show typedefs ++ "\n"
            putStrLn $ "Functions:\n" ++ show functions ++ "\n"
    where
        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
