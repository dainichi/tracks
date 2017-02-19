import Perm(permutation)
import Data.Dequeue(BankersDequeue)
import Data.Foldable(concatMap,toList)
import PaulTrack(PaulTrack(S,L,R),closed,cords)
import Vec(Vec,UVec)
import Tra(TRTrack)

closedPerm::BankersDequeue PaulTrack -> Bool
closedPerm tdq = closed(mconcat (map cords (toList tdq)::[TRTrack (Vec Double)(UVec Double)]))

printSolution s = concatMap show (toList s) ++ "\n"

main = let trs c s = filter closedPerm (permutation (replicate (12+c) L ++ replicate c R ++ replicate s S))
           pr c s = let t = trs c s
                    in do
                        putStrLn $ show (12+c) ++ show L ++ ", " ++ show c ++ show R ++ ", " ++ show s ++ show S ++ ", " ++ show (length t) ++ " solutions"
                        putStr (concatMap printSolution t)
       in sequence [ pr c s | c <- [3,2..0] , s <- [5,4..0]] 
