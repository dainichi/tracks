{-# LANGUAGE MultiParamTypeClasses #-}
module Tra where
import Data.Monoid

class RotPos r p where
    (<**>) :: r -> p -> p

data TRTrack t r = TRTrack t r deriving (Show)

instance (Monoid t, Monoid r, RotPos r t) => Monoid (TRTrack t r) where
    TRTrack t1 r1 `mappend` TRTrack t2 r2 = TRTrack(t1 <> (r1 <**> t2))(r1 <> r2)
    mempty = TRTrack mempty mempty

