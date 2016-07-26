module RunLength (encode, decode) where 

import Data.List (groupBy)
import Data.Char (isDigit)

encode :: String -> String
encode = foldl (++) ""
       . map (encodeBlock)
       . groupBy (==)
  where encodeBlock as@(a:_) = (show $ length as) ++ [a]

decode :: String -> String
decode = concat
       . expand
       . groupBy (bothNumeric) 
  where bothNumeric a b = (isDigit a) && (isDigit b)
        expand [] = []
        expand (num:letter:pairs) = replicate (read num) (head letter) : expand pairs
