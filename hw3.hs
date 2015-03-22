{-# OPTIONS_GHC -Wall #-}
module Golf where

  skips :: [a] -> [[a]]
  skips xs = fmap (subSkips xs) nths
    where nths = [1..length xs]
          subSkips :: [a] -> Int -> [a]
          subSkips l n = case drop (n - 1) l of
                          []     -> []
                          (a:as) -> a : subSkips as n


  localMaxima :: [Integer] -> [Integer]
  localMaxima (a:b:c:xs) = compareThree [a,b,c] ++ localMaxima xs
      where compareThree :: [Integer] -> [Integer]
            compareThree (x:y:z:[]) = if y > x && y > z then [y] else []
            compareThree _ = []
  localMaxima _ = []

  histogram :: [Integer] -> String
  

