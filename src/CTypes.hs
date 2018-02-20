module CTypes where

-- TODO: allow several brace styles

data CHeader = CHeader CIncludeGuard [CIncludedHeader] CStruct

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

data CType = CUintT Int


instance Show CHeader where
    show (CHeader (CIncludeGuard c) hdrs s) = "#ifndef " ++ c
        ++ "\n#define " ++ c ++ "\n\n"
        ++ concat (map show hdrs) ++ "\n\n"
        ++ show s
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
            lineFeed s = s ++ "\n"

instance Show CField where
    show (CField t n) = show t ++ " " ++ n ++ ";"

instance Show CType where
    show (CUintT n) = "uint" ++ show n ++ "_t"
