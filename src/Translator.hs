module Translator (translate) where

import Synt
import CTypes
import Utils (abbreviate)

translate :: PDescription -> CContent
translate (PDescription str) = CContent (toUserTypes str) (toFunctions str)

toUserTypes :: PStruct -> [CTypeDef]
toUserTypes (PStruct name fields) = [CTypeDef (CStruct name) (map toStructField fields)]
-- TODO: check for equal variable names

toStructField :: PField -> CVar
toStructField (PField name typ) = CVar (toCType typ) name

toCType :: PType -> CType
toCType (PType (PBitFieldType PUimsbf bits)) = CUintT (toCTypeBits bits)

toCTypeBits :: Int -> Int
toCTypeBits n
    | n <= 0 || n > 64 = error $ "Cannot have " ++ show n ++ " bits"
    | n <= 8 = 8
    | otherwise = 2 ^ (ceiling (logBase 2 (fromIntegral n)))

toFunctions :: PStruct -> [CFunction]
toFunctions (PStruct name _) = map ($ name) [parseFunction, freeFunction]

parseFunction :: String -> CFunction
parseFunction structName = CFunction parseFunctionHeader [] -- TODO: instructions
    where
        parseFunctionHeader = CFuncHeader CBoolT (structName ++ "_parse") [dat, len, res]
        dat = CVar (CPtrT $ CConstT $ CUintT 8) "data"
        len = CVar CSizeT "length"
        res = CVar (CPtrT $ CPtrT $ CUserT $ CStruct structName) (abbreviate structName)

freeFunction :: String -> CFunction
freeFunction structName = CFunction freeFunctionHeader [] -- TODO: instructions
    where
        freeFunctionHeader = CFuncHeader CVoidT (structName ++ "_free") [ps]
        ps = CVar (CPtrT (CUserT (CStruct structName))) (abbreviate structName)
