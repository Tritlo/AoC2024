module Main where
import Data.List (partition, findIndices)


main :: IO ()
main = do lns <- map (map (read @Int) . words) . lines
                <$> readFile "input.txt"
          let pairs (a:b:r) = (a,b) : pairs (b:r)
              pairs _ = []
              cf a b = a > b && a - b <= 3
              c l@((a,b):_) | a > b = all (uncurry cf) l
                            | otherwise = all (uncurry $ flip cf) l
              (g,b) = partition (c . pairs)  lns
              wo1 (a:r) = (r:) $ map (a:) $ wo1 r
              wo1 _ = []
              cwo1 = any (c . pairs) . wo1
          let gl = length g
              rl = length (filter cwo1 b)
          print gl
          print $ gl + rl
