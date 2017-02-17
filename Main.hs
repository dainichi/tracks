import Perm
import Data.Dequeue
import Data.Foldable
import PaulTrack
import Vec
import Tra

closedPerm::BankersDequeue PaulTrack -> Bool
closedPerm tdq = closed(mconcat (map cords (toList tdq)::[TRTrack (Vec Double)(UVec Double)]))

main = let trs c s = filter closedPerm (permutation (replicate (12+c) L ++ replicate c R ++ replicate s S))
           pr c s = let t = trs c s
                    in do
                        putStrLn $ show $ t
                        putStrLn $ show $ length t
       in do
            pr 3 5
            pr 3 4    
            pr 3 3    
            pr 3 2    
            pr 3 1    
            pr 3 0    
            pr 2 5    
            pr 2 4    
            pr 2 3    
            pr 2 2    
            pr 2 1    
            pr 2 0    
            pr 1 5    
            pr 1 4    
            pr 1 3    
            pr 1 2    
            pr 1 1    
            pr 1 0    
            pr 0 5
            pr 0 4
            pr 0 3
            pr 0 2
            pr 0 1
            pr 0 0
