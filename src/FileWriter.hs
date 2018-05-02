module FileWriter (writeHeader, writeSource) where

import System.FilePath (addExtension, dropExtension)
import CTypes

writeHeader :: FilePath -> CContent -> IO ()
writeHeader inputFileName _ = writeFile headerFileName "<header content>"
    where headerFileName = getFileName inputFileName "h"

writeSource :: FilePath -> CContent -> IO ()
writeSource inputFileName _ = writeFile sourceFileName "<source content>"
    where sourceFileName = getFileName inputFileName "c"


getFileName :: FilePath -> String -> FilePath
getFileName inputFileName extension =
    addExtension (baseFileName ++ "_parser") extension
    where baseFileName = dropExtension inputFileName
