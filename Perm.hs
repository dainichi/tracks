module Perm where
import Data.Dequeue (empty, popFront, pushBack, Dequeue)
import Data.List (nub, delete)

lc xs ys = 
    case (popFront xs, popFront ys) of
        (Just (x,xx), Just (y,yy)) -> 
            case compare x y of
                EQ -> lc xx yy
                ord -> ord
        _ -> EQ

perm acc nym [] = 
    case lc acc nym of
        LT -> []
        EQ -> [acc]
        _ -> error $ show acc ++ " " ++ show nym
perm acc nym xs = do
    x <- nub xs
    case popFront nym of
        Just (n,nn) ->
            case compare x n of
                LT -> []
                EQ -> perm (pushBack acc x) (pushBack nn x) (delete x xs)
                GT -> let pb = pushBack acc x in perm pb pb (delete x xs)
        _ -> let pb = pushBack acc x in perm pb pb (delete x xs)

permutation::(Ord a,Show (q a),Dequeue q)=>[a]->[q a]
permutation = perm empty empty

