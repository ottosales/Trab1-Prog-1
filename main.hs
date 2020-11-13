import System.Random
import System.IO
import Data.Char
import Data.List as List (sort)

import Data.Text as Text (Text, pack, unpack, splitOn)
-- Input Reading

type DataTuple = ([Double], String)

main = do
    dataList <- readInput
    -- outputFileName <- readOutput
    percTest <- readPercentual
    seed <- readSeed
    let testSize = calcTestSize (read percTest) (length dataList)
    let randomList = genRandom (read seed) testSize (length dataList)
    let trainingGroup = getTrainingList dataList randomList
    let testGroup = getTestList dataList randomList

    let centroids = calcAllCentroids dataList
    print centroids

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

removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | x `elem` ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)

genRandom :: Int -> Int -> Int -> [Int]
genRandom seed testSize limit = take testSize (removeDup (randomRs (0,limit) $ mkStdGen seed:: [Int]))

calcTestSize :: Integral a => a -> a -> a
calcTestSize percTest testSize = (percTest * testSize) `div` 100

getTrainingList :: [a] -> [Int] -> [a]
getTrainingList dataList randomList = [dataList !! x | x <- randomList]

getTestList :: [a] -> [Int] -> [a]
getTestList dataList randomList = [dataList !! x | x <- [0..length dataList - 1], x `notElem` randomList]

getClassList :: Eq a2 => [(a1, a2)] -> [a2]
getClassList dataList = removeDup [snd x | x <- dataList]

getAllElementsInClass :: [([Double], String)] -> String -> [[Double]]
getAllElementsInClass dataList className = [ fst x | x <- dataList, snd x == className] 

calcCentroid :: [[Double]] -> [Double]
calcCentroid [] = []
calcCentroid [element] = element
calcCentroid (element : elements) = zipWith (+) element (calcCentroid elements)

calcAllCentroids :: [([Double], String)] -> [([Double], String)]
calcAllCentroids dataList = [makeTuple (map (/ fromIntegral (length (getAllElementsInClass dataList x))) (calcCentroid (getAllElementsInClass dataList x))) x | x <- getClassList dataList]

calcDist :: ([Double], String) -> ([Double], String) -> Double
calcDist (xs, _) (ys, _) = sqrt . sum $ [uncurry (-) z ** 2 | z <- zip xs ys]