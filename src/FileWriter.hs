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
                                            joinTexts typedefStrs,
                                            "",
                                            joinStrings funcHeaderStrs,
                                            "",
                                            endifStr,
                                            ""]
    where
        headerFileName = getFileName inputFileName "h"
        typedefStrs = map (joinStrings . textify) typedefs
        (ifndefStr, defineStr, endifStr) = getIncludeGuard headerFileName
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
addTypeInclude s (CPtrT typ) = addTypeInclude s typ
addTypeInclude s (CConstT typ) = addTypeInclude s typ
addTypeInclude s (CUintT _) = Set.insert "stdint.h" s
addTypeInclude s CBoolT = Set.insert "stdbool.h" s
addTypeInclude s CSizeT = Set.insert "stddef.h" s
addTypeInclude s CResultT = Set.insert "parsley.h" s
addTypeInclude s _ = s

writeSource :: FilePath -> CContent -> IO ()
writeSource inputFileName (CContent _ functions) =
    writeFile sourceFileName $ joinStrings [joinStrings sourceIncudeStrs,
                                            "",
                                            joinTexts functionStrs,
                                            ""]
    where
        sourceFileName = getFileName inputFileName "c"
        functionStrs = map (joinStrings . textify) functions
        headerFileName = getFileName inputFileName "h"
        sourceIncudes = getSourceIncludes headerFileName functions
        sourceIncudeStrs = map makeInclude $ Set.toList sourceIncudes

-- TODO: remove files included in header
getSourceIncludes :: FilePath -> [CFunction] -> Set.Set String
getSourceIncludes headerFileName functions =
    foldl' f1 (Set.fromList ["parsley_bitstream.h", headerFileName]) functions
    where
        f1 s (CFunction header instructions) =
            Set.union (getFuncHeaderIncludes header) (foldl' f2 s instructions)
        f2 s (CVarD (CVarDecl typ _)) = addTypeInclude s typ
        f2 s (CRV (CFuncCall "malloc" _)) = Set.insert "stdlib.h" s
        f2 s (CRV (CFuncCall "free" _)) = Set.insert "stdlib.h" s
        f2 s _ = s

getFileName :: FilePath -> String -> FilePath
getFileName inputFileName extension =
    addExtension (baseFileName ++ "_parser") extension
    where baseFileName = dropExtension inputFileName

makeInclude :: FilePath -> String
makeInclude fileName = "#include <" ++ fileName ++ ">"
