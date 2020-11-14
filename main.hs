import System.Random ( mkStdGen, Random(randomRs) )
import Data.List as List (sort, elemIndex)
import Data.Maybe as Maybe (fromMaybe)
import Data.Text as Text (Text, pack, unpack, splitOn)
import Text.Printf (printf)

type DataTuple = ([Double], String)

main :: IO ()
main = do
    --input/output/percentual/seed reading
    dataList <- readInput
    outputFileName <- readOutput
    percTest <- readPercentual
    seed <- readSeed
    
    -- generating random numbers, creating and spliting lists
    let testSize = calcTestSize (read percTest) (length dataList)
    let randomList = genRandom (read seed) testSize (length dataList)
    let trainingGroup = getTrainingList dataList randomList
    let testGroup = getTestList dataList randomList
    let centroids = calcAllCentroids trainingGroup

    -- using closest neighbour method
    let neighbour = classifyAllItems trainingGroup testGroup
    -- using closest centroid method
    let centroid = classifyAllItems centroids testGroup
    
    -- printing accuracy
    printf "Acuracia(vizinho): %.2f%%\n" $ calcAccuracy (rightCount testGroup neighbour 0) (length testGroup)
    printf "Acuracia(centroide): %.2f%%\n" $calcAccuracy (rightCount testGroup centroid 0) (length testGroup)
    
    -- calculating confusion table
    let confTableNeighbour = confusionTable neighbour testGroup (getClassList testGroup)
    let confTableCentroid = confusionTable centroid testGroup (getClassList testGroup)
    
    -- printing confusion table to output file
    writeFile outputFileName ""
    appendFile outputFileName "vizinho mais próximo:\n"
    appendFile outputFileName $ printConfTable confTableNeighbour
    appendFile outputFileName "\ncentroides:\n"
    appendFile outputFileName $ printConfTable confTableCentroid

    --all done :) end of main


-- reads the input and returns a list of tuples, with the fst element of the tuple being the point (a list of Double) and the second a string (point's class)
readInput :: IO [([Double], String)]
readInput = do
    putStrLn "Forneca o nome do arquivo de entrada:"
    fileName <- getLine
    handle <- readFile fileName
    return [lineToData handle | handle <- lines handle]

-- splits the line at the ","
splitLine :: String -> [Text]
splitLine line = Text.splitOn (Text.pack ",") (Text.pack line)

-- gets just the element's point, doing the right conversions
getDoubles :: Read a => [Text] -> [a]
getDoubles line = [read (Text.unpack x) | x <- init line]

-- gets just the element's class
getClass :: String -> String
getClass line = Text.unpack (last (splitLine line))

-- wraps all the previous functions, converting a string to useful data
lineToData :: String -> ([Double],String)
lineToData line = makeTuple (getDoubles (splitLine line)) (getClass line)

-- creates a tuple
makeTuple :: a -> b -> (a, b)
makeTuple x y = (x, y)

-- simple output reading function
readOutput :: IO String
readOutput = do
    putStrLn "Forneca o nome do arquivo de saida:"
    getLine

-- simple percentual reading function
readPercentual :: IO String
readPercentual = do
    putStrLn "Forneca o percentual de exemplos de teste:"
    getLine

-- simple seed reading function
readSeed :: IO String
readSeed = do
    putStrLn "Forneca o valor da semente para geracao randomizada:"
    getLine

-- teacher's function to remove duplicate values from a list
removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | x `elem` ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)

-- generates a list of random numbers given a seed, a size and a limit to the values
genRandom :: Int -> Int -> Int -> [Int]
genRandom seed testSize limit = take testSize (removeDup (randomRs (0,limit-1) $ mkStdGen seed:: [Int]))

-- calculates the size of the test list
calcTestSize :: Integral a => a -> a -> a
calcTestSize percTest testSize = (percTest * testSize) `div` 100

-- gets the test list
getTestList :: [([Double], String)] -> [Int] -> [([Double], String)]
getTestList dataList randomList = [dataList !! x | x <- randomList]

-- gets the training list
getTrainingList :: [([Double], String)] -> [Int] -> [([Double], String)]
getTrainingList dataList randomList = [dataList !! x | x <- [0..length dataList - 1], x `notElem` randomList]

-- get a list of all classes in a given list
getClassList :: Ord a1 => [(a2, a1)] -> [a1]
getClassList dataList = sort $ removeDup [snd x | x <- dataList]

-- gets all elements of a given class
getAllElementsInClass :: [([Double], String)] -> String -> [[Double]]
getAllElementsInClass dataList className = [ fst x | x <- dataList, snd x == className] 

-- calculates a centroid of given points
calcCentroid :: [[Double]] -> [Double]
calcCentroid [] = []
calcCentroid [element] = element
calcCentroid (element : elements) = zipWith (+) element (calcCentroid elements)

--calculates all centroids
calcAllCentroids :: [([Double], String)] -> [([Double], String)]
calcAllCentroids dataList = [makeTuple (map (/ fromIntegral (length (getAllElementsInClass dataList x))) (calcCentroid (getAllElementsInClass dataList x))) x | x <- getClassList dataList]

--calculates a distance between two points
calcDist :: ([Double], String) -> ([Double], String) -> Double
calcDist (xs, _) (ys, _) = sqrt . sum $ [uncurry (-) z ** 2 | z <- zip xs ys]

-- function that tries to guess a point's class with a given training group
guessClassPoint :: [([Double], String)] -> ([Double], String) -> ([Double], String)
guessClassPoint trainingGroup point = makeTuple (fst point) (snd (trainingGroup !! Maybe.fromMaybe (-1) (elemIndex (minimum list) list))) 
    where
        list = [calcDist x point | x <- trainingGroup]

-- classifies all points of a given test group
classifyAllItems :: [([Double], String)] -> [([Double], String)] -> [([Double], String)]
classifyAllItems trainingGroup testGroup = [ guessClassPoint trainingGroup x | x <- testGroup]

-- counts the number off total right guesses the program did
rightCount :: [([Double], String)] -> [([Double], String)] -> Int -> Int
rightCount [] [] value = value
rightCount (x:xs) (y:ys) value =
    if snd x == snd y then
        rightCount xs ys (succ value)
        else rightCount xs ys value

-- calculates the accuracy of the program
calcAccuracy :: Int -> Int -> Double
calcAccuracy right total = fromIntegral (100 * right) / fromIntegral total

-- informs the number of times the program guessed a class "guessedClass" and the right class was "rightClass", usefull with the function below
calcGuesses :: [([Double], String)] -> [([Double], String)] -> String -> String -> Int -> Int
calcGuesses [] [] _ _ value = value
calcGuesses  (x:xs) (y:ys) guessedClass rightClass value =
    if snd x == guessedClass && snd y == rightClass then
        calcGuesses xs ys guessedClass rightClass (succ value)
        else calcGuesses xs ys guessedClass rightClass value

-- calculates the whole confusion table (aka confusion matrix)
confusionTable :: [([Double], String)] -> [([Double], String)] -> [String] -> [[Int]]
confusionTable guessedGroup testGroup classes = [[ calcGuesses guessedGroup testGroup x y 0 | y <- classes ] | x <- classes]

-- debug function, useful to check every value in a list of elements :)
makeReadable :: [([Double], String)] -> String
makeReadable xs = unlines $ [show x | x <- xs]

-- right align a number, and transforms it into a string
fixNumberPosition :: Int -> String
fixNumberPosition x
    | x > 9    = show x
    | otherwise = " " ++ show x

-- creates a line to be shown in the confusion table
createConfTableLine :: [String] -> [Char] -> [Char]
createConfTableLine [] line = line
createConfTableLine [x] _ = x
createConfTableLine (x:xs) line = line ++ x ++ ", " ++ createConfTableLine xs line

-- displays the confusion table
printConfTable :: [[Int]] -> String
printConfTable confTable = unlines [" " ++ createConfTableLine [fixNumberPosition x | x <- y] "" | y <- confTable]

-- work's done :)