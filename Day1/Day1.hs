module Main where
import Data.List (sort)
import Data.IntMap qualified as IM

main :: IO ()
main = do inp <- unzip . (map ( both read 
                              . fmap (drop 2)
                              . span (/= ' '))
                              . lines) <$> readFile "input.txt"
          print $ sum $ map (\(a,b) -> max a b - min a b)
                $ uncurry zip $ both sort inp
          print $ sum $ IM.elems
                $ uncurry (IM.intersectionWithKey (\k a b -> k * a * b))
                $ both count inp
  where count :: [Int] -> IM.IntMap Int
        count = foldl (\m x -> IM.insertWith (+) x 1 m) IM.empty
        both f (a,b) = (f a, f b)
