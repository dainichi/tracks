import Data.List (nub, delete)

permutations acc _ [] = [acc]
permutations acc ns [l] = 
    if lt (l:ns) acc then [] else [acc++[l]]
permutations acc [] xs = do
    x <- nub xs
    permutations (acc++[x]) (acc++[x]) (delete x xs)
permutations acc (n:ns) xs = do
    x <- nub xs
    if x < n 
    then []
    else if x == n 
         then permutations (acc++[x]) (ns++[x]) (delete x xs)
         else permutations (acc++[x]) (acc++[x]) (delete x xs)

lt (x:xs)(y:ys) = x < y || (x == y && lt xs ys)
lt _ _ = False

