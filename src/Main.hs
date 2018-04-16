import System.Environment (getArgs, getProgName)
import Data.List (intersperse)
-- import System.FilePath (addExtension, dropExtension)
import Lex
import Synt
import CTypes
import Translator
import Textifier

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (inputFileName:_) -> do
            contents <- readFile inputFileName
            let tree = synt $ alexScanTokens contents
            let (result @ (CContent typedefs functions)) = translate tree
            putStrLn $ "Typedefs:\n" ++ show typedefs ++ "\n"
            putStrLn $ "Functions:\n" ++ show functions ++ "\n"
            putStrLn $ "String representation:\n\n" ++ (concat $ intersperse "\n" $ textify result)
    where
        {-writeFiles inputFileName header = do
            let baseFileName = (dropExtension inputFileName) ++ "_parser"
            let parserHeaderFileName = addExtension baseFileName "h"
            writeFile parserHeaderFileName $ show header-}

        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
