module Format (format) where

import Data.List (intersperse)

format :: [String] -> String
format [] = ""
format ss = concat $ intersperse "\n" $ tabulate 0 $ map spacify $ stringify ss

stringify :: [String] -> [[String]]
stringify [] = []
stringify (s:ss) = reverse $ map reverse $ stringify' ss [[s]]
    where
        stringify' :: [String] -> [[String]] -> [[String]]
        stringify' [] acc = acc
        stringify' (s:ss) (a:aa)
            | s == "{" || s == ";" = stringify' ss ([]:(s:a):aa)
            | otherwise = stringify' ss ((s:a):aa)

spacify :: [String] -> String
spacify [] = ""
spacify (s:ss) = concat $ s : (spacify' ss)
    where
        spacify' [] = []
        spacify' (";":ss) = ";" : (spacify' ss)
        spacify' (s:ss) = " ":s:(spacify' ss)

-- TODO: refactore
tabulate :: Int -> [String] -> [String]
tabulate _ [] = []
tabulate n (s:ss)
    | not (null s) && last s == '{' = tabulate' n s : tabulate (succ n) ss
    | not (null s) && head s == '}' = let predn = pred n in
                                      tabulate' predn s : tabulate predn ss
    | otherwise = tabulate' n s : tabulate n ss
    where
        tabulate' n s = concat (replicate n tab) ++ s
        tab = "    " -- TODO: get from a config file
