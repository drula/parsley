import System.Environment (getArgs, getProgName)
import System.FilePath (addExtension, dropExtension)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showUsage
        (inputFileName:_) -> writeFiles inputFileName

    where
        writeFiles inputFileName = do
            let baseFileName = dropExtension inputFileName
            let baseParserFileName = baseFileName ++ "_parser"
            let parserHeaderFileName = addExtension baseParserFileName "h"
            let parserSourceFileName = addExtension baseParserFileName "c"
            writeFile parserHeaderFileName "parser header\n"
            writeFile parserSourceFileName "parser source\n"

        showUsage = do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " file.prl"
