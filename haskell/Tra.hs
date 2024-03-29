{-# LANGUAGE MultiParamTypeClasses #-}
module Tra where

class RotPos r p where
    (<**>) :: r -> p -> p

data TRTrack t r = TRTrack t r deriving (Show)

instance (Semigroup t, Semigroup r, RotPos r t) => Semigroup (TRTrack t r) where
    TRTrack t1 r1 <> TRTrack t2 r2 = TRTrack(t1 <> (r1 <**> t2))(r1 <> r2)

instance (Monoid t, Monoid r, RotPos r t) => Monoid (TRTrack t r) where
    mempty = TRTrack mempty mempty

