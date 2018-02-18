module HeaderGen (createHeader) where

import Synt

-- TODO: save in a configuration file
modulePrefix = "parsley"
tab = "    "

-- TODO: allow several brace styles
-- TODO: check if the number of bits is a multiple of 8
createHeader :: Description -> String
createHeader (Description (Struct name fields))
    = "struct " ++ (prefixed $ name) ++ " {\n"
    ++ fieldList ++ "};\n"
    where
        fieldList = concat $ map (tabbed . lineFeed . fieldToStr) fields

-- TODO: move to common utilities module
prefixed :: String -> String
prefixed s = modulePrefix ++ "_" ++ s

tabbed :: String -> String
tabbed = (++) tab

lineFeed :: String -> String
lineFeed s = s ++ "\n"

fieldToStr :: Field -> String
fieldToStr (Field name typ) = typeToCType typ ++ " " ++ name ++ ";"

typeToCType :: Type -> String
typeToCType (Type (BitFieldType typ bits)) = numTypeToCType typ
    ++ show (bitsToCTypeBits bits) ++ "_t"
    where numTypeToCType Uimsbf = "uint"

-- TODO: show the name of the variable in case of error
bitsToCTypeBits :: Int -> Int
bitsToCTypeBits n
    | n <= 0 || n > 64 = error $ "Cannot have " ++ show n ++ " bits"
    | n <= 8 = 8
    | otherwise = 2 ^ (ceiling (logBase 2 (fromIntegral n)))
