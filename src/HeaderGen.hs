module HeaderGen (createHeader) where

import Data.Char (toUpper)
import qualified Data.Set as Set
import Synt
import CTypes

-- TODO: save in a configuration file
modulePrefix = "parsley"

-- TODO: check if the number of bits is a multiple of 8
createHeader :: PDescription -> CHeader
createHeader (PDescription (s @ (PStruct n _)))
    = CHeader (createIncludeGuard n) (createIncludedHeaders fs) struct (createFunctionDecls struct n)
    where (struct @ (CStruct _ fs)) = createStruct s

createIncludeGuard :: String -> CIncludeGuard
createIncludeGuard name = CIncludeGuard guardConstant
    where guardConstant = map toUpper $ prefixed $ name ++ "_H"

createStruct :: PStruct -> CStruct
createStruct (PStruct name fs) = CStruct (prefixed name) $ createFields fs

-- TODO: move to common utilities module
prefixed :: String -> String
prefixed s = modulePrefix ++ "_" ++ s

createFields :: [PField] -> [CField]
createFields fs = map createField fs

createField :: PField -> CField
createField (PField n t) = CField (createType t) n

createType :: PType -> CType
createType (PType (PBitFieldType PUimsbf bits)) = CUintT $ bitsToCTypeBits bits
    where
        bitsToCTypeBits n
            | n <= 0 || n > 64 = error $ "Cannot have " ++ show n ++ " bits"
            | n <= 8 = 8
            | otherwise = 2 ^ (ceiling (logBase 2 (fromIntegral n)))

createIncludedHeaders :: [CField] -> [CIncludedHeader]
createIncludedHeaders fs = map CIncludedHeader $ Set.toList $ foldr fun Set.empty fs
    where
        fun (CField (CUintT _) _) s = Set.insert "stdint.h" s
        fun (CField t _) _ = error $ "createIncludedHeaders is not implemented for " ++ show t

-- TODO: refactore
createFunctionDecls :: CStruct -> String -> [CFunctionDecl]
createFunctionDecls (CStruct n _) n' = [parseFunc, freeFunc]
    where
        parseFunc = CFunctionDecl CBool (prefixedName ++ "_parse")
            [CField (CPointer (CUintT 8)) "data",
             CField CSizeT "length",
             CField (CDoublePointer (CUserT typ)) abbreviatedName]
        freeFunc = CFunctionDecl CVoid (prefixedName ++ "_free")
            [CField (CPointer (CUserT typ)) abbreviatedName]
        prefixedName = n
        typ = prefixedName ++ "_t"
        abbreviatedName = abbreviate n'

abbreviate :: String -> String
abbreviate "" = ""
abbreviate (c:cs) = c : abbreviate (skip cs)
    where
        skip "" = ""
        skip ('_':cs) = cs
        skip (_:cs) = skip cs
