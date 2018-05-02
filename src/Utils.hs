module Utils where

import Data.List (intercalate)

abbreviate :: String -> String
abbreviate "" = ""
abbreviate (c:cs) = c : abbreviate (skip cs)
    where
        skip "" = ""
        skip ('_':cs) = cs
        skip (_:cs) = skip cs

joinStrings :: [String] -> String
joinStrings = intercalate "\n"
