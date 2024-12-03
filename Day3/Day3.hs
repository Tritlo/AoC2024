module Main where

import qualified Data.Char as C
import qualified Text.Read as TR
import qualified Data.Either as E

main :: IO ()
main = do lns <- readFile "input.txt"
          let b (Left i) = i
              b (Right i) = i
          print $ sum $ map b $ findMul' Right lns
          print $ sum $ E.rights $ findMul' Right lns
          
findMul' :: (Int -> Either Int Int) -> String -> [Either Int Int]
findMul' f ('m':'u':'l':'(':xs) |
            (d1@(_:_),',':r1) <- span C.isDigit xs,
            (d2@(_:_),')':r2) <- span C.isDigit r1,
            Just n1 <- TR.readMaybe d1,
            Just n2 <- TR.readMaybe d2
    = f (n1*n2) : findMul' f r2
findMul' _ ('d':'o':'(':')':xs) = findMul' Right xs
findMul' _ ('d':'o':'n':'\'':'t':'(':')':xs) = findMul' Left xs
findMul' b (_:xs) = findMul' b xs
findMul' _ [] = []
