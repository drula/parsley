module HeaderGen (createHeader) where

import Data.Char (toUpper)
import qualified Data.Set as Set
import Synt
import CTypes

-- TODO: save in a configuration file
modulePrefix = "parsley"

-- TODO: check if the number of bits is a multiple of 8
createHeader :: Description -> CHeader
createHeader (Description (s @ (Struct n _)))
    = CHeader (createIncludeGuard n) (createIncludedHeaders fs) struct (createFunctionDecls struct)
    where (struct @ (CStruct _ fs)) = createStruct s

createIncludeGuard :: String -> CIncludeGuard
createIncludeGuard name = CIncludeGuard guardConstant
    where guardConstant = map toUpper $ prefixed $ name ++ "_H"

createStruct :: Struct -> CStruct
createStruct (Struct name fs) = CStruct (prefixed name) $ createFields fs

-- TODO: move to common utilities module
prefixed :: String -> String
prefixed s = modulePrefix ++ "_" ++ s

createFields :: [Field] -> [CField]
createFields fs = map createField fs

createField :: Field -> CField
createField (Field n t) = CField (createType t) n

createType :: Type -> CType
createType (Type (BitFieldType Uimsbf bits)) = CUintT $ bitsToCTypeBits bits
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
createFunctionDecls :: CStruct -> [CFunctionDecl]
createFunctionDecls (CStruct n _) = [parseFunc, freeFunc]
    where
        parseFunc = CFunctionDecl CBool (prefixedName ++ "_parse")
            [CField (CPointer (CUintT 8)) "data",
             CField CSizeT "length",
             CField (CDoublePointer (CUserT typ)) "s"] -- TODO
        freeFunc = CFunctionDecl CVoid (prefixedName ++ "_free")
            [CField (CPointer (CUserT typ)) "s"] -- TODO
        prefixedName = n
        typ = prefixedName ++ "_t"
