module Golf where

skips :: [a] -> [[a]]
skips n  = concat
       . map (map head . takeWhile (not . null) . iterate (drop n))
