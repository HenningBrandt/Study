-- copied from stackoverflow. review later to reenact
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where 
    combinations' n k' l@(y:ys)
        | k' == 0   = [[]]
        | k' >= n   = [l]
        | null l    = []
        | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys

cardinality :: [a] -> Int
cardinality = length

subset :: Eq a => [a] -> [a] -> [a]
subset xs ys = filter (flip elem $ ys) xs

subset' :: Eq a => [[a]] -> [a]
subset' = foldl1 subset

subsetCardinality :: Eq a => [[a]] -> Int
subsetCardinality = cardinality . subset'

sieveMethod' :: Eq a => Int -> Int -> [[a]] -> Int
sieveMethod' acc k xs
  | k == length xs = acc + factor * subsetCardinality xs
  | otherwise = sieveMethod' (acc + factor * s) (k + 1) xs
  where factor = if k `mod` 2 == 0 then -1 else 1
        comb = combinations k xs
        s = sum $ map subsetCardinality comb

sieveMethod :: Eq a => [[a]] -> Int
sieveMethod xs 
  | null xs = 0
  | length xs == 1 = cardinality (head xs)
  | otherwise = union + sieveMethod' 0 2 xs
  where union = sum $ map cardinality xs
          





