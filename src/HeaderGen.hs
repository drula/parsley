module HeaderGen (createHeader) where

import Synt
import CTypes

-- TODO: save in a configuration file
modulePrefix = "parsley"

-- TODO: check if the number of bits is a multiple of 8
createHeader :: Description -> CHeader
createHeader (Description s) = CHeader $ createStruct s

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
