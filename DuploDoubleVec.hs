{-# LANGUAGE FlexibleInstances #-}
import Perm(permutation)
import Data.Dequeue(BankersDequeue)
import Data.Foldable(concatMap,toList)
import Duplo(DuploPiece(S,L,R),DuploImpl,impl,closed)
import Vec
import Tra

instance DuploImpl (TRTrack (Vec Double)(UVec Double)) where
    impl S = TRTrack (Vec 1 0)(UVec 1 0)
    impl L = TRTrack (Vec 1 (2-sqrt(3))) (UVec (sqrt(3)/2) (1/2))
    impl R = TRTrack (Vec 1 (sqrt(3)-2)) (UVec (sqrt(3)/2) (-1/2))
    closed (TRTrack (Vec a b) (UVec c d)) =
        abs a < 0.01 && abs b < 0.01 && abs(c-1) < 0.01 && abs d < 0.01

closedPerm tdq = closed(mconcat (map impl (toList tdq)::[TRTrack (Vec Double)(UVec Double)]))

printSolution s = concatMap show (toList s) ++ "\n"

main = let pr c s = let t = filter closedPerm (permutation (replicate (12+c) L ++ replicate c R ++ replicate s S)::[BankersDequeue DuploPiece])
                    in do
                        putStrLn $ show (12+c) ++ show L ++ ", " ++ show c ++ show R ++ ", " ++ show s ++ show S ++ ", " ++ show (length t) ++ " solutions"
                        putStr (concatMap printSolution t)
       in sequence [ pr c s | c <- [3,2..0] , s <- [5,4..0]] 
