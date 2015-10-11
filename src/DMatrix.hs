module DMatrix where
import qualified Data.List as L
import qualified Data.List.Utils as LU

strToFloat :: String -> Float
strToFloat = read

headers :: [[String]] -> [String]
headers = head

body :: [[String]] -> [[String]]
body xs = tail $ init xs

index :: Int -> [[String]] -> [String]
index index xs = map (!! index) (body xs)

indexByRow :: String -> [[String]] -> Maybe Int
indexByRow row xs = L.elemIndex h xs
                  where h = headers xs 

row :: String -> [[String]] -> [String]
row row xs = case indexByRow row xs of
  Just i -> index i xs
  Nothing -> []

factorize :: [String] -> [Float]
factorize xs = map strToFloat $
  map (LU.replace "setosa" "0") $
  map (LU.replace "versicolor" "1") $
  map (LU.replace "virginica" "2") xs
