module Translator (translate) where

import Synt
import CTypes
import Utils (abbreviate)

translate :: PDescription -> CContent
translate (PDescription str) = CContent (toUserTypes str) (toFunctions str)

toUserTypes :: PStruct -> [CTypeDef]
toUserTypes (PStruct name fields) = [CTypeDef (CStruct (name ++ "_t"))
    (map toStructField fields)]
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
parseFunction (s @ (PStruct name _)) = CFunction parseFunctionHeader $
    parseFuncInstrs s
    where
        parseFunctionHeader = CFuncHeader CResultT (name ++ "_parse") [dat, len, res]
        dat = CVarDecl (CPtrT $ CConstT $ CUintT 8) "data"
        len = CVarDecl CSizeT "length"
        res = CVarDecl (CPtrT $ CPtrT $ CUserT $ CStruct (name ++ "_t")) ("out_" ++ abbreviate name)

parseFuncInstrs :: PStruct -> [CInstruction] -- TODO
parseFuncInstrs (PStruct name fields)
    = [CVarD $ CVarDecl CResultT res,
       CEmpty,
       CVarD $ CVarDecl CBitStreamT bs,
       CRV $ CFuncCall "bs_init" [addrBs, "data", "length"],
       CEmpty,
        CIfElse (CFuncCall "bs_has_bytes" [addrBs, show (streamLength fields)])
            [CVarD $ CVarDecl (CPtrT $ CUserT $ CStruct (name ++ "_t")) ms,
                CAssignment ms (CFuncCall "malloc" ["sizeof *" ++ ms]),
                CIfElse (CJust "ms != NULL")
                    (parseFields ms fields ++
                    [CEmpty,
                    CAssignment ("*out_" ++ ms) (CJust ms),
                    CAssignment res (CJust "PRL_RESULT_OK")])
                    [CAssignment res (CJust "PRL_RESULT_MEMORY_ERROR")]
            ]
            [CAssignment res (CJust "PRL_RESULT_WRONG_SIZE")],
        CEmpty,
        CReturn res]
    where
        res = "res"
        bs = "bs"
        addrBs = "&" ++ bs
        ms = abbreviate name

parseFields :: String -> [PField] -> [CInstruction]
parseFields _ [] = []
parseFields strName (f:fs) = (CAssignment fullFiedlName
    (CFuncCall (getReadFuncName fieldBits) ["&bs", show fieldBits])) :
        (parseFields strName fs)
    where
        fullFiedlName = strName ++ "->" ++ (fieldName f)
        fieldBits = bits $ getType $ fieldType f

getReadFuncName :: Int -> String
getReadFuncName n = "bs_read_bits_to_u" ++ show (toCTypeBits n)

streamLength :: [PField] -> Int
streamLength fields = if rest == 0
                      then bytes
                      else error "the number of bits must be a multiple of 8"
    where
        (bytes, rest) = divMod (sum $ map f fields) 8
        f (PField _ (PType (PBitFieldType _ bits))) = bits

freeFunction :: PStruct -> CFunction
freeFunction (PStruct name _) = CFunction freeFunctionHeader instructions
    where
        freeFunctionHeader = CFuncHeader CVoidT (name ++ "_free") [ps]
        ps = CVarDecl (CPtrT (CUserT (CStruct (name ++ "_t")))) ms
        ms = abbreviate name
        instructions = [CRV $ CFuncCall "free" [ms]]
