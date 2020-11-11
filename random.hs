import System.IO
import System.Random

tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs

removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)

main :: IO ()
main = do let g = mkStdGen 42
          let aleatorios = take 50 (removeDup (
                      (randomRs (1,150) g :: [Int])))
          print aleatorios

-- [49,96,16,146,24,140,51,134,143,42,95,93,92,128,67,10,76,123,25,75,32,35,73,108,126,120,39,4,98,87,81,78,66,59,132,136,133,106,30,14,28,19,45,29,99,70,116,147,41,84]