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
toCType (PBitFieldType PUimsbf bits) = CUintT (toCTypeBits bits)
toCType (PStringType size) = CCharArrayT (succ size) -- FIXME: overflow

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
                    (parseFields 0 ms fields ++
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

parseFields :: Int -> String -> [PField] -> [CInstruction]
parseFields _ _ [] = []
parseFields offset strName (f:fs) = (getReadFunction f) : (parseFields (getNewOffset f) strName fs)
    where
        fullFiedlName = strName ++ "->" ++ (fieldName f)
        getReadFunction (PField _ (PBitFieldType _ n)) =
            CAssignment fullFiedlName (CFuncCall ("bs_read_bits_to_u" ++ show (toCTypeBits n)) ["&bs", show n])
        getReadFunction (PField name (PStringType n)) =
            if offset `mod` 8 == 0
            then CRV (CFuncCall "bs_read_zero_string" ["&bs", fullFiedlName, show n])
            else error $ "the beginning of the string `" ++ name ++ "` is not located at the beginning of byte"
        getNewOffset (PField _ (PBitFieldType _ n)) = offset + n
        getNewOffset (PField _ (PStringType n)) = offset + n * 8

streamLength :: [PField] -> Int
streamLength fields = if rest == 0
                      then bytes
                      else error "the number of bits must be a multiple of 8"
    where
        (bytes, rest) = divMod (sum $ map f fields) 8
        f (PField _ (PBitFieldType _ bits)) = bits
        f (PField _ (PStringType size)) = size * 8

freeFunction :: PStruct -> CFunction
freeFunction (PStruct name _) = CFunction freeFunctionHeader instructions
    where
        freeFunctionHeader = CFuncHeader CVoidT (name ++ "_free") [ps]
        ps = CVarDecl (CPtrT (CUserT (CStruct (name ++ "_t")))) ms
        ms = abbreviate name
        instructions = [CRV $ CFuncCall "free" [ms]]
