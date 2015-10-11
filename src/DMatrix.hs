module DMatrix where
import qualified Data.List as L
import qualified Text.Read as R
import qualified Data.Maybe as D

strToFloat :: String -> Float
strToFloat s = case R.readMaybe s :: Maybe Float of
  Just i -> realToFrac i :: Float
  Nothing -> 9.9

headers :: [[String]] -> [String]
headers = head

body :: [[String]] -> [[String]]
body xs = tail $ init xs

index :: Int -> [[String]] -> [String]
index i xs = map (!! i) (body xs)

indexByRow :: String -> [[String]] -> Maybe Int
indexByRow row xs = L.elemIndex row (headers xs) 

row :: String -> [[String]] -> [String]
row r xs = case indexByRow r xs of
  Just i -> index i xs
  Nothing -> []

factorize :: [String] -> [Float]
factorize xs = map fn xs where
  fn x = case L.elemIndex x (L.nub xs) of
    Just i -> realToFrac i :: Float
    Nothing -> 9.9

rowIsFactors :: [String] -> Bool
rowIsFactors r = and $ map D.isNothing $
  map (R.readMaybe :: String -> Maybe Float) r

convertRow :: [String] -> [Float]
convertRow r = case rowIsFactors r of
  False -> map strToFloat r
  True -> factorize r

toDMatrix :: [[String]] -> [[Float]]
toDMatrix xs = map convertRow (L.transpose $ body xs)
