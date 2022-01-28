module Perm where
import Data.Dequeue (empty, popFront, pushBack, Dequeue)
import Data.List (nub, delete)

--the class represents lists/bags that allow an operation this returns distinct elements paired with the rest. Nub is the traditional name to get distinct elements
class NubDel f where
    nubDel::Eq a => f a -> [(a, f a)]

instance NubDel [] where
    nubDel l = do
                x <- nub l
                return (x, delete x l)

newtype Bag a = Bag [(a,Int)] deriving (Show)

instance NubDel Bag where
    nubDel (Bag []) = []
    nubDel (Bag ((x,1):l)) = (x,Bag l):map (\(y,Bag m) -> (y, Bag ((x,1):m))) (nubDel (Bag l))
    nubDel (Bag ((x,n):l)) | n > 0 = (x,Bag ((x,n-1):l)): map (\(y,Bag m) -> (y, Bag ((x,n):m))) (nubDel (Bag l))
                               | n == 0 = nubDel (Bag l)


cyclic xs ys = 
    case (popFront xs, popFront ys) of
        (Just (x,xx), Just (y,yy)) -> x==y && cyclic xx yy
        _ -> True

--a permutation algorithm which only generates the lexicographically smallest rotation, i.e. a cononical form representing the cyclic permutation. It makes sure that no subsequence is lexicographically smaller than the whole sequence generated so far. nym (not yet matched) is the remainder of the currently generated sequence when removing the longest subsequence currently generated which equals a prefix of the currently generated sequence. I have not rigidly proved correctness.
perm acc nym aeqn l = 
    let nubDels = nubDel l
    in if null nubDels 
       then   if aeqn || cyclic acc nym 
              then [acc]
              else []
       else do
                (x,xx) <- nubDels
                let Just (n,nn) = popFront nym
                case compare x n of
                    LT -> []
                    EQ -> perm (pushBack acc x) (pushBack nn x) False xx
                    GT -> let pb = pushBack acc x in perm pb pb True xx

necklaces l = do
                    (x,xx) <- nubDel l
                    let pex = pushBack empty x
                    perm pex pex True xx

