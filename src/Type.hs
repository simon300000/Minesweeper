module Type where

data Block = Edge | Show Space | Hide Space deriving (Eq, Show)

data Space = Bomb | Explode | Number Number | Empty deriving (Eq, Show)

data Number = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Show)

instance Enum Number where
  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum 5 = Five
  toEnum 6 = Six
  toEnum 7 = Seven
  toEnum 8 = Eight
  toEnum _ = error "Number: toEnum: invalid argument"
  fromEnum One = 0
  fromEnum Two = 1
  fromEnum Three = 2
  fromEnum Four = 3
  fromEnum Five = 4
  fromEnum Six = 5
  fromEnum Seven = 6
  fromEnum Eight = 7
