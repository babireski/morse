{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UnicodeSyntax #-}

module Morse where

import Data.Tuple
import Encodable

data Signal = Dit | Dah deriving (Eq)
data Morse = Morse [Signal] deriving (Eq)
type Encoding = [(Char, Morse)]

instance Show Signal where
    show Dit = "·"
    show Dah = "−"

instance Read Signal where
    readsPrec _ "·" = [(Dit, "")]
    readsPrec _ "−" = [(Dah, "")]

instance Show Morse where
    show (Morse a) = a >>= show

instance Read Morse where
    readsPrec _ code = [(Morse [signal | symbol ← code, signal ← read [symbol]], "")]

international ∷ Encoding
international =
    [
        ('A', Morse [Dit, Dah]), 
        ('B', Morse [Dah, Dit, Dit, Dit]),
        ('C', Morse [Dah, Dit, Dah, Dit]),
        ('D', Morse [Dah, Dit, Dit]),
        ('E', Morse [Dit]),
        ('F', Morse [Dit, Dit, Dah, Dit]),
        ('G', Morse [Dah, Dah, Dit]),
        ('H', Morse [Dit, Dit, Dit, Dit]),
        ('I', Morse [Dit, Dit]),
        ('J', Morse [Dit, Dah, Dah, Dah]),
        ('K', Morse [Dah, Dit, Dah]),
        ('L', Morse [Dit, Dah, Dit, Dit]),
        ('M', Morse [Dah, Dah]),
        ('N', Morse [Dah, Dit]),
        ('O', Morse [Dah, Dah, Dah]),
        ('P', Morse [Dit, Dah, Dah, Dit]),
        ('Q', Morse [Dah, Dah, Dit, Dah]),
        ('R', Morse [Dit, Dah, Dit]),
        ('S', Morse [Dit, Dit, Dit]),
        ('T', Morse [Dah]),
        ('U', Morse [Dit, Dit, Dah]),
        ('V', Morse [Dit, Dit, Dit, Dah]),
        ('W', Morse [Dit, Dah, Dah]),
        ('X', Morse [Dah, Dit, Dit, Dah]),
        ('Y', Morse [Dah, Dit, Dah, Dah]),
        ('Z', Morse [Dah, Dah, Dit, Dit]),
        ('1', Morse [Dit, Dah, Dah, Dah, Dah]),
        ('2', Morse [Dit, Dit, Dah, Dah, Dah]),
        ('3', Morse [Dit, Dit, Dit, Dah, Dah]),
        ('4', Morse [Dit, Dit, Dit, Dit, Dah]),
        ('5', Morse [Dit, Dit, Dit, Dit, Dit]),
        ('6', Morse [Dah, Dit, Dit, Dit, Dit]),
        ('7', Morse [Dah, Dah, Dit, Dit, Dit]),
        ('8', Morse [Dah, Dah, Dah, Dit, Dit]),
        ('9', Morse [Dah, Dah, Dah, Dah, Dit]),
        ('0', Morse [Dah, Dah, Dah, Dah, Dah]),
        ('0', Morse [Dah])
    ]

instance Encodable Encoding Char Morse where
    encode encoding char = lookup char encoding
    decode encoding morse = lookup morse (map swap encoding)

instance Encodable Encoding String [Morse] where
    encode encoding string = sequence $ map (encode encoding) string
    decode encoding morse = sequence $ map (decode encoding) morse