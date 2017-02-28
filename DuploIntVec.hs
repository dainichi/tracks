{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}
import Perm(permutation)
import Data.Dequeue(BankersDequeue)
import Duplo(DuploPiece(S,L,R),DuploImpl,impl,closed)
import IntVec30deg
import Tra

instance DuploImpl (TRTrack IntTra30 IntAng30) where
    impl S = TRTrack (IntTra30 1 1 0 0)(IntAng30 0)
    impl L = TRTrack (IntTra30 0 2 2 2) (IntAng30 1)
    impl R = TRTrack (IntTra30 2 0 (-2)(-2)) (IntAng30 (-1))
    closed (TRTrack (IntTra30 a b c d) (IntAng30 e)) =
        a==0 && b==0 && c==0 && d==0 && e `mod` 12 == 0

--note: foldMap is a concatMap generalized to Foldable, so we can handle Dequeues without converting to lists.

closedPerm tdq = closed(foldMap (impl::_->TRTrack IntTra30 IntAng30) tdq)

printSolution s = foldMap show s ++ "\n"

main = let pr c s = let t = filter closedPerm (permutation (replicate (12+c) L ++ replicate c R ++ replicate s S)::[BankersDequeue _])
                    in do
                        putStrLn $ show (12+c) ++ show L ++ ", " ++ show c ++ show R ++ ", " ++ show s ++ show S ++ ", " ++ show (length t) ++ " solutions"
                        putStr (concatMap printSolution t)
       in sequence [ pr c s | c <- [3,2..0] , s <- [5,4..0]] 
