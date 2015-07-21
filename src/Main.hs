module Main where

import Language.TurkishDeasciifier

main :: IO ()
main = fmap deasciify getContents >>= putStr
