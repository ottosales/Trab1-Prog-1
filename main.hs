import System.IO  
import Data.Char

import Data.Text as Text (Text, pack, unpack, splitOn)
-- Input Reading

type Coords = Double
type Class = String
type DataTuple = ([Coords], Class)

main = do
    infoList <- readInput
    
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

    