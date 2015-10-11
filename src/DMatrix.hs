module DMatrix where
import qualified Data.List as L

strToFloat :: String -> Float
strToFloat = read

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
    Just i -> realToFrac i
    Nothing -> 9.9
