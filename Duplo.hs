module Duplo where

--implementing types should respect the duplo layout, e.g.
--12 L/R curves to make a circle with radius 2S
data DuploPiece = L | R | S deriving (Show, Eq, Ord)

class DuploImpl t where
    impl::DuploPiece->t
    closed::t->Bool
