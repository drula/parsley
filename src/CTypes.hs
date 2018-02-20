module CTypes where

import Data.List (intersperse)

-- TODO: allow several brace styles

data CHeader = CHeader CIncludeGuard [CIncludedHeader] CStruct [CFunctionDecl]

-- TODO: CIncludedGuard should contain the stuff that is inside it
data CIncludeGuard = CIncludeGuard { headerConstant :: String }

data CIncludedHeader = CIncludedHeader { headerName :: String }

data CStruct = CStruct {
    structName :: String,
    fields :: [CField]
}

data CField = CField {
    fieldType :: CType,
    fieldName :: String
}

data CType = CVoid | CUintT Int | CBool | CSizeT
           | CPointer CType | CDoublePointer CType
           | CUserT String

isPointer :: CType -> Bool
isPointer (CPointer _) = True
isPointer (CDoublePointer _) = True
isPointer _ = False

data CFunctionDecl = CFunctionDecl {
    returnType :: CType,
    funcName   :: String,
    params     :: [CField]
}


instance Show CHeader where
    show (CHeader (CIncludeGuard c) hdrs s fds) = "#ifndef " ++ c
        ++ "\n#define " ++ c ++ "\n\n"
        ++ concat (map show hdrs) ++ "\n\n"
        ++ show s ++ "\n\n"
        ++ concat (intersperse "\n" (map show fds))
        ++ "\n\n#endif // " ++ c ++ "\n"
        -- according to C standard, every file must ends with a line feed character

-- CIncludeGuard can only be shown as part of CHeader

instance Show CIncludedHeader where
    show (CIncludedHeader name) = "#include <" ++ name ++ ">"

instance Show CStruct where
    show (CStruct n fs) = "typedef struct " ++ n ++ " {\n"
        ++ fields
        ++ "} " ++ n ++ "_t;"
        where
            fields = concat $ map (tabulate . lineFeed . show) fs
            tabulate s = tab ++ s
            tab = "    " -- TODO: get from a config file
            lineFeed s = s ++ ";\n"

instance Show CField where
    show (CField t n)
        | isPointer t = show t ++ n
        | otherwise = show t ++ " " ++ n

instance Show CType where
    show CVoid = "void"
    show (CUintT n) = "uint" ++ show n ++ "_t"
    show CBool = "bool"
    show CSizeT = "size_t"
    show (CPointer t) = show t ++ " *"
    show (CDoublePointer t) = show t ++ " **"
    show (CUserT n) = n

instance Show CFunctionDecl where
    show (CFunctionDecl rt fn ps) = show rt ++ " " ++ fn ++ "("
        ++ concat (intersperse ", " (map show ps)) ++ ");"
