module FileWriter (writeHeader, writeSource) where

import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Set as Set
import System.FilePath (addExtension, dropExtension)

import CTypes
import Textifier
import Utils

writeHeader :: FilePath -> CContent -> IO ()
writeHeader inputFileName (CContent typedefs functions) =
    writeFile headerFileName $ joinStrings [ifndefStr,
                                            defineStr,
                                            "",
                                            joinStrings includeStrs,
                                            "",
                                            joinStrings typedefStrs,
                                            "",
                                            joinStrings funcHeaderStrs,
                                            "",
                                            endifStr,
                                            ""]
    where
        headerFileName = getFileName inputFileName "h"
        typedefStrs = map (joinStrings . textify) typedefs
        (ifndefStr, defineStr, endifStr) = getIncludeGuard headerFileName
        makeInclude file = "#include <" ++ file ++ ">"
        funcHeaders = map getFuncHeader functions
        funcHeaderStrs = map ((++ ";") . stringify) funcHeaders
        funcHeaderIncludes = Set.unions $ map getFuncHeaderIncludes funcHeaders
        typedefIncludes = Set.unions $ map getTypedefIncludes typedefs
        includes = Set.union funcHeaderIncludes typedefIncludes
        includeStrs = map makeInclude $ Set.toList includes

getIncludeGuard :: FilePath -> (String, String, String)
getIncludeGuard fileName = ("#ifndef " ++ fileNameConstant,
                            "#define " ++ fileNameConstant,
                            "#endif // " ++ fileNameConstant)
    where
        fileNameConstant = map toGuardChar fileName
        toGuardChar '.' = '_'
        toGuardChar c = toUpper c

getTypedefIncludes :: CTypeDef -> Set.Set String
getTypedefIncludes (CTypeDef _ vardecls) = foldl' f Set.empty vardecls
    where f s (CVarDecl typ _ ) = addTypeInclude s typ

getFuncHeader :: CFunction -> CFuncHeader
getFuncHeader (CFunction header _) = header

getFuncHeaderIncludes :: CFuncHeader -> Set.Set String
getFuncHeaderIncludes (CFuncHeader retType _ vardecls) =
    addTypeInclude (foldl' f Set.empty vardecls) retType
    where f s (CVarDecl typ _ ) = addTypeInclude s typ

addTypeInclude :: Set.Set String -> CType -> Set.Set String
addTypeInclude s (CUintT _) = Set.insert "stdint.h" s
addTypeInclude s CBoolT = Set.insert "stdbool.h" s
addTypeInclude s CResultT = Set.insert "parsley.h" s
addTypeInclude s _ = s

writeSource :: FilePath -> CContent -> IO ()
writeSource inputFileName (CContent _ functions) =
    writeFile sourceFileName $ joinStrings functionStrs
    where
        sourceFileName = getFileName inputFileName "c"
        functionStrs = map (joinStrings . textify) functions


getFileName :: FilePath -> String -> FilePath
getFileName inputFileName extension =
    addExtension (baseFileName ++ "_parser") extension
    where baseFileName = dropExtension inputFileName
