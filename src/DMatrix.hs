module DMatrix where
import qualified Data.List as L
import qualified Text.Read as R
import qualified Data.Maybe as D

data DataFrame = DataFrame {
  headers :: [String], body :: [[String]] } deriving (Show)

type DMatrix = [[Float]]


unique :: [String] -> [String]
unique = L.nub


strToFloat :: String -> Float
strToFloat s = case R.readMaybe s :: Maybe Float of
  Just i -> realToFrac i :: Float
  Nothing -> 9.9


listBody :: [[String]] -> [[String]]
listBody xs = tail $ init xs


index :: Int -> DataFrame -> [String]
index i df = map (!! i) (body df)


indexByColumn :: String -> DataFrame -> Maybe Int
indexByColumn col df = L.elemIndex col (headers df) 


column :: String -> DataFrame -> [String]
column colname df = case indexByColumn colname df of
  Just i -> index i df
  Nothing -> []


factorize :: [String] -> [Float]
factorize xs = map fn xs where
  fn x = case L.elemIndex x (unique xs) of
    Just i -> realToFrac i :: Float
    Nothing -> 9.9


dummyLists :: [String] -> [[String]]
dummyLists col = map fn (unique col) where
  fn x = [ if j == x then "1" else "0" | j <- col ]


processColumn :: [String] -> [[String]]
processColumn col = case columnIsFactors col of
  False -> [col] --map strToFloat c
  True -> dummyLists col


dummyDf :: [[String]] -> [[[String]]]
dummyDf []     = []
dummyDf (x:xs) = (processColumn x) : (dummyDf xs)


data Column = Float [Float] | Character [String]
type Mungestep = [Column] -> [Column]

munge :: [Mungestep] -> [Column] -> [Column]
munge []     df = df
munge (x:xs) df = munge xs $ munge1 x df

munge1 :: Mungestep -> [Column] -> [Column]
munge1 m df = foldl (++) (m df) []


columnIsFactors :: [String] -> Bool
columnIsFactors col = or $ map D.isNothing $
  map (R.readMaybe :: String -> Maybe Float) col


seqAlong :: [a] -> [Int]
seqAlong xs = [0..(length xs -1)]


--TODO: I ended up not needing this, but I don't want to get rid of it.
which :: [Bool] -> [Int]
which xs = filter (\x -> x /= -1)
  [ if (xs !! j) == True then j else -1 | j <- seqAlong xs ]


convertColumn :: [String] -> [Float]
convertColumn col = case columnIsFactors col of
  False -> map strToFloat col
  True -> factorize col


toDMatrix :: DataFrame -> DMatrix
toDMatrix xs = map convertColumn (L.transpose $ body xs)
