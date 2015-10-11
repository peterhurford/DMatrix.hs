module DMatrix where
import qualified Data.List as L
import qualified Text.Read as R
import qualified Data.Maybe as D

data DataFrame = DataFrame {
  headers :: [String], body :: [[String]] } deriving (Show)

type DMatrix = [[Float]]


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
indexByColumn column df = L.elemIndex column (headers df) 


column :: String -> DataFrame -> [String]
column c df = case indexByColumn c df of
  Just i -> index i df
  Nothing -> []


factorize :: [String] -> [Float]
factorize xs = map fn xs where
  fn x = case L.elemIndex x (unique xs) of
    Just i -> realToFrac i :: Float
    Nothing -> 9.9


dummyLists :: [String] -> [[String]]
dummyLists column = map fn (unique column) where
  fn x = [ if j == x then "1" else "0" | j <- column ]


processColumn :: [String] -> [[String]]
processColumn c = case columnIsFactors c of
  False -> [c] --map strToFloat c
  True -> dummyLists c


dummyDf :: [[String]] -> [[[String]]]
dummyDf []     = []
dummyDf (x:xs) = (processColumn x) : (dummyDf xs)


-- class Mungeable [a] where
--   munge1 :: [a] -> [[b]]

-- instance Mungeable [String] where
--   munge1 :: [String] -> [[b]]

data Column = Float [Float] | Character [String]
type Mungestep = Column -> [Column]

munge :: [Mungestep] -> DataFrame -> DataFrame
munge []     df = df
munge (x:xs) df = munge xs $ munge1 x $ body df

munge1 :: Mungestep -> DataFrame -> DataFrame
munge1 m df = foldl (++) (m df) []


columnIsFactors :: [String] -> Bool
columnIsFactors c = or $ map D.isNothing $
  map (R.readMaybe :: String -> Maybe Float) c


seqAlong :: [a] -> [Int]
seqAlong xs = [0..(length xs -1)]


--TODO: I ended up not needing this, but I don't want to get rid of it.
which :: [Bool] -> [Int]
which xs = filter (\x -> x /= -1)
  [ if (xs !! j) == True then j else -1 | j <- seqAlong xs ]


convertColumn :: [String] -> [Float]
convertColumn c = case columnIsFactors c of
  False -> map strToFloat c
  True -> factorize c


toDMatrix :: DataFrame -> DMatrix
toDMatrix xs = map convertColumn (L.transpose $ body xs)
