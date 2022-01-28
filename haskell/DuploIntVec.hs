{-# LANGUAGE FlexibleInstances, PartialTypeSignatures #-}
import Perm(necklaces,Bag(..))
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

closedPieceList pieceList = closed(foldMap (impl::_->TRTrack IntTra30 IntAng30) pieceList)

printPieceList pieces = do
                    mapM_ (putStr . show) pieces
                    putStr "\n"

main = 
   let printSolutions rightPieces straightPieces = 
        let leftPieces = rightPieces + 12
            closedPieceLists = filter closedPieceList (necklaces (Bag [(L, leftPieces),(R,rightPieces),(S,straightPieces)])::[BankersDequeue _])
        in do mapM_ printPieceList closedPieceLists
              mapM_ putStr [show leftPieces, show L, ", ", show rightPieces, show R, ", ", show straightPieces, show S, ", ", show (length closedPieceLists), " solutions\n"]
   in sequence_ [ printSolutions rightPieces straightPieces | rightPieces <- [3,2..0] , straightPieces <- [5,4..0]] 
