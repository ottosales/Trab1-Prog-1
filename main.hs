import System.Random
import System.IO
import Data.Char
import Data.List as List (sort)

import Data.Text as Text (Text, pack, unpack, splitOn)
-- Input Reading

type Coords = Double
type Class = String
type DataTuple = ([Coords], Class)

main = do
    dataList <- readInput
    -- outputFileName <- readOutput
    percTest <- readPercentual
    let testSize = calcTestSize (read percTest) (length dataList)
    seed <- readSeed
    randomList <- genRandom (read seed) testSize (length dataList)
    print randomList
    let xs = sort randomList
    print xs

    putStrLn "all finished!"

readInput :: IO [([Double], String)]
readInput = do
    putStrLn "Please inform the input file name:"
    fileName <- getLine
    handle <- readFile fileName
    return [lineToData handle | handle <- lines handle]

splitLine :: String -> [Text]
splitLine line = Text.splitOn (Text.pack ",") (Text.pack line)

getDoubles :: Read a => [Text] -> [a]
getDoubles line = [read (Text.unpack x) | x <- init line]

getClass :: String -> String
getClass line = Text.unpack (last (splitLine line))

lineToData :: String -> ([Double],String)
lineToData line = makeTuple (getDoubles (splitLine line)) (getClass line)

makeTuple :: a -> b -> (a, b)
makeTuple x y = (x, y)


readOutput :: IO String
readOutput = do
    putStrLn "Please inform the output file name:"
    getLine

readPercentual :: IO String
readPercentual = do
    putStrLn "Please inform the examples percentual:"
    getLine

readSeed :: IO String
readSeed = do
    putStrLn "Please inform the seed for the random program:"
    getLine

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

genRandom seed testSize limit= do 
    let g = mkStdGen seed
    let aleatorios = take testSize (removeDup (randomRs (0,limit) g :: [Int]))
    return aleatorios


calcTestSize percTest testSize = (percTest * testSize) `div` 100

