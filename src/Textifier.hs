module Textifier (textify) where

import Data.List (intersperse)
import CTypes

class Textified a where
    textify :: a -> [String]

class Stringified a where
    stringify :: a -> String

tabulate :: String -> String
tabulate s = "    " ++ s

instance Textified CContent where
    textify (CContent typedefs functions) =
        (concat $ intersperse [""] $ map textify typedefs) ++ [""] ++
        (concat $ intersperse [""] $ map textify functions)

instance Textified CTypeDef where
    textify (CTypeDef (CStruct name) vardecls) = ["typedef struct {"]
        ++ map (tabulate . (++ ";") . stringify) vardecls
        ++ ["} " ++ name ++ ";"]

instance Stringified CVarDecl where
    stringify (CVarDecl (ptrType @ (CPtrT _)) name) = stringify ptrType ++ name
    stringify (CVarDecl typ name) = stringify typ ++ " " ++ name

instance Stringified CType where
    stringify CVoidT = "void"
    stringify CBoolT = "bool"
    stringify (CUintT n) = "uint" ++ show n ++ "_t"
    stringify CSizeT = "size_t"
    stringify (CUserT (CStruct name)) = name -- FIXME: with "struct"?
    stringify CResultType = "result_t"
    stringify CBitStreamT = "bitstream_t"
    stringify (CConstT (CPtrT typ)) = stringify typ ++ " * const" -- constant pointer (int * const)
    stringify (CConstT typ) = "const " ++ stringify typ -- also pointer to constant
    stringify (CPtrT (ptrType @ (CPtrT _))) = stringify ptrType ++ "*"
    stringify (CPtrT typ) = stringify typ ++ " *"


instance Textified CFunction where
    textify (CFunction header instuctions) = [stringify header ++ " {"]
        ++ (map tabulate $ concatMap textify instuctions)
        ++ ["}"]

instance Stringified CFuncHeader where
    stringify (CFuncHeader resType name params) = stringify resType
        ++ " " ++ name ++ "(" ++ (concat $ intersperse ", " $ map stringify params) ++ ")"

instance Textified CInstruction where
    textify (CVarD vardecl) = [stringify vardecl ++ ";"]
    textify (CRV rvalue) = [stringify rvalue ++ ";"]
    textify (CAssignment var rvalue) = [var ++ " = " ++ stringify rvalue ++ ";"]
    textify (CIfElse cond ifInstructions elseInstructions) =
        ["if (" ++ (stringify cond) ++ ") {"]
        ++ (map tabulate $ concatMap textify ifInstructions)
        ++ ["}", "else {"]
        ++ (map tabulate $ concatMap textify elseInstructions)
        ++ ["}"]
    textify (CReturn value) = ["return " ++ value ++ ";"]
    textify CEmpty = [""]

instance Stringified CRValue where
    stringify (CJust value) = value
    stringify (CFuncCall name params) = name ++ "(" ++ (concat $ intersperse ", " params) ++ ")"