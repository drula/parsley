module CTypes where

-- TODO: allow several brace styles

data CHeader = CHeader CStruct

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
    show (CHeader s) = show s ++ "\n" -- according to C standard, every file
                                      -- must ends with a line feed character

instance Show CStruct where
    show (CStruct n fs) = "struct " ++ n ++ " {\n"
        ++ fields
        ++ "};"
        where
            fields = concat $ map (tabulate . lineFeed . show) fs
            tabulate s = tab ++ s
            tab = "    " -- TODO: get from a config file
            lineFeed s = s ++ "\n"

instance Show CField where
    show (CField t n) = show t ++ " " ++ n ++ ";"

instance Show CType where
    show (CUintT n) = "uint" ++ show n ++ "_t"
