{-# LANGUAGE FunctionalDependencies, UnicodeSyntax #-}

module Encodable where

class Encodable a b c | a b → c where
    encode ∷ a → b → Maybe c
    decode ∷ a → c → Maybe b