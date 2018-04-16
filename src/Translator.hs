module Translator (translate) where

import Synt
import CTypes
import Utils (abbreviate)

translate :: PDescription -> CContent
translate (PDescription str) = CContent (toUserTypes str) (toFunctions str)

toUserTypes :: PStruct -> [CTypeDef]
toUserTypes (PStruct name fields) = [CTypeDef (CStruct name) (map toStructField fields)]
-- TODO: check for equal variable names

toStructField :: PField -> CVarDecl
toStructField (PField name typ) = CVarDecl (toCType typ) name

toCType :: PType -> CType
toCType (PType (PBitFieldType PUimsbf bits)) = CUintT (toCTypeBits bits)

toCTypeBits :: Int -> Int
toCTypeBits n
    | n <= 0 || n > 64 = error $ "Cannot have " ++ show n ++ " bits"
    | n <= 8 = 8
    | otherwise = 2 ^ (ceiling (logBase 2 (fromIntegral n)))

toFunctions :: PStruct -> [CFunction]
toFunctions s = map ($ s) [parseFunction, freeFunction]

parseFunction :: PStruct -> CFunction
parseFunction (s @ (PStruct name _)) = CFunction parseFunctionHeader $ parseFuncInstrs s
    where
        parseFunctionHeader = CFuncHeader CResultType (name ++ "_parse") [dat, len, res]
        dat = CVarDecl (CPtrT $ CConstT $ CUintT 8) "data"
        len = CVarDecl CSizeT "length"
        res = CVarDecl (CPtrT $ CPtrT $ CUserT $ CStruct name) (abbreviate name)

parseFuncInstrs :: PStruct -> [CInstruction] -- TODO
parseFuncInstrs (PStruct name fields)
    = [CVarD $ CVarDecl CResultType res,
        CIfElse (CCondition $ "length == " ++ show (streamLength fields))
            [CVarD $ CVarDecl (CPtrT $ CUserT $ CStruct name) ms,
                {-CFuncCall-}
                CAssignment res "RESULT_OK"]
            [CAssignment res "RESULT_WRONG_SIZE"],
        CReturn res]
    where
        res = "res"
        ms = abbreviate name

streamLength :: [PField] -> Int
streamLength fields = if rest == 0
                      then bytes
                      else error "the number of bits must be a multiple of 8"
    where
        (bytes, rest) = divMod (sum $ map f fields) 8
        f (PField _ (PType (PBitFieldType _ bits))) = bits

freeFunction :: PStruct -> CFunction
freeFunction (PStruct name _) = CFunction freeFunctionHeader [] -- TODO: instructions
    where
        freeFunctionHeader = CFuncHeader CVoidT (name ++ "_free") [ps]
        ps = CVarDecl (CPtrT (CUserT (CStruct name))) (abbreviate name)
