module Type where

data Block = Edge | Show Space | Hide Space

data Space = Bomb | Explode | Number Number | Empty

data Number = One | Two | Three | Four | Five | Six | Seven | Eight
