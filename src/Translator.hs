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
toCType (PFixedStringType size) = CCharArrayT (succ size) -- FIXME: overflow
toCType (PSizedStringType _) = CPtrT CCharT

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

data ParseState = Initial | Parse | CheckSize

parseInstructions :: [PField] -> ParseState -> [CInstruction]
parseInstructions [] _ =
    [
        CAssignment ("*out_" ++ ms) (CJust ms),
        CReturn "PRL_RESULT_OK"
    ]
    where
        ms = "ms" -- FIXME: ugly hack

parseInstructions fs CheckSize =
    [
        CIfElse (CFuncCall "bs_has_bytes" ["&bs", show (toBytes $ getKnownPartBitSize fs 0)])
        (parseInstructions fs Parse )
        [CAssignment res (CJust "PRL_RESULT_WRONG_SIZE")]
    ]
    where res = "res"

-- FIXME: check the size of fields group
parseInstructions ((PField name (PBitFieldType _ bits)):fs) Parse =
    (CAssignment fullFieldName (CFuncCall ("bs_read_bits_to_u" ++ show (toCTypeBits bits)) ["&bs", show bits]))
    : (parseInstructions fs Parse)
    where fullFieldName = "ms->" ++ name

parseInstructions ((PField name (PFixedStringType n)):fs) Parse =
    (CRV (CFuncCall "bs_read_zero_string" ["&bs", fullFieldName, show n]))
    : (parseInstructions fs Parse)
    where fullFieldName = "ms->" ++ name

parseInstructions ((PField name (PSizedStringType sizeFieldName)):fs) Parse =
    [
        CIfElse (CFuncCall "bs_has_bytes" ["&bs", fullSizeFieldName])
        [
            CAssignment fullFieldName (CFuncCall "malloc" [fullSizeFieldName ++ " + 1"]),
            CIfElse (CJust $ fullFieldName ++ " != NULL")
            (
                ((CRV $ CFuncCall "bs_read_zero_string" ["&bs", fullFieldName, fullSizeFieldName])
                : (parseInstructions fs CheckSize)) ++
                [CRV $ CFuncCall "free" [fullFieldName]]
                -- TODO: check if "free" is really needed (if it isn't after "return OK")
            )
            [
                CAssignment res (CJust "PRL_RESULT_MEMORY_ERROR")
            ]
        ]
        [
            CAssignment res (CJust "PRL_RESULT_WRONG_SIZE")
        ]
    ]
    where
        fullSizeFieldName = "ms->" ++ sizeFieldName
        fullFieldName = "ms->" ++ name
        res = "res"

parseInstructions fs Initial =
    [
        CVarD $ CVarDecl CResultT res,
        CEmpty,
        CVarD $ CVarDecl CBitStreamT bs,
        CRV $ CFuncCall "bs_init" [addrBs, "data", "length"],
        CEmpty,
        CIfElse (CFuncCall "bs_has_bytes" [addrBs, show (toBytes $ getKnownPartBitSize fs 0)])
        [
            CVarD $ CVarDecl (CPtrT $ CUserT $ CStruct ms_type) ms,
            CAssignment ms (CFuncCall "calloc" ["1", "sizeof *" ++ ms]),
            CIfElse (CJust "ms != NULL")
            (
                (parseInstructions fs Parse) ++
                [CEmpty,
                CRV $ CFuncCall "free" [ms]]
            )
            [
                CAssignment res (CJust "PRL_RESULT_WRONG_SIZE")
            ]
        ]
        [
            CAssignment res (CJust "PRL_RESULT_WRONG_SIZE")
        ],
        CEmpty,
        CReturn res
    ]
    where
        bs = "bs"
        addrBs = "&" ++ bs
        res = "res"
        ms = "ms" -- FIXME: ugly hack
        ms_type = "main_struct_t" -- FIXME: ugly hack

toBytes :: Int -> Int
toBytes bitSize = if bitSize `mod` 8 == 0
                  then bitSize `div` 8
                  else error "the bit size is not a multiple of 8" -- FIXME: more descriptive message

getKnownPartBitSize :: [PField] -> Int -> Int
getKnownPartBitSize [] bitSize = bitSize
getKnownPartBitSize ((PField _ (PBitFieldType _ bits)) : fs) bitSize = getKnownPartBitSize fs (bitSize + bits)
getKnownPartBitSize ((PField _ (PFixedStringType size)) : fs) bitSize = getKnownPartBitSize fs (bitSize + size * 8)
getKnownPartBitSize ((PField _ (PSizedStringType _)) : _) bitSize = bitSize

parseFuncInstrs :: PStruct -> [CInstruction]
parseFuncInstrs (PStruct name fields) = parseInstructions fields Initial

-- TODO: free inner fields
freeFunction :: PStruct -> CFunction
freeFunction (PStruct name fields) = CFunction freeFunctionHeader (getFreeInstructions fields)
    where
        freeFunctionHeader = CFuncHeader CVoidT (name ++ "_free") [ps]
        ps = CVarDecl (CPtrT (CUserT (CStruct (name ++ "_t")))) ms
        ms = abbreviate name

        getFreeInstructions :: [PField] -> [CInstruction]
        getFreeInstructions fields =
            [
                CIfElse (CJust $ ms ++ "!= NULL")
                ((getFreeFieldsInstructions fields) ++ [CRV $ CFuncCall "free" [ms]])
                []
            ]

        getFreeFieldsInstructions :: [PField] -> [CInstruction]
        getFreeFieldsInstructions [] = []
        getFreeFieldsInstructions ((PField name (PSizedStringType _)):fs) =
            (CRV $ CFuncCall "free" [ms ++ "->" ++ name])
            : (getFreeFieldsInstructions fs)
        getFreeFieldsInstructions (_:fs) = getFreeFieldsInstructions fs

