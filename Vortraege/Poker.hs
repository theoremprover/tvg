{-# LANGUAGE UnicodeSyntax,RecordWildCards,FlexibleInstances,TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Poker where

import Data.Maybe


data Suit = Spades | Hearts | Diamonds | Clubs
	deriving (Enum,Bounded,Ord,Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Enum,Bounded,Ord,Eq)

type Card = (Value,Suit)
type Deck = [Card]

deck = [ (value,suit) | value <- [Two .. Ace], suit <- [Spades,Hearts,Diamonds,Clubs] ]

instance Show Suit where
	show Spades   = "♠"
	show Hearts   = "♥"
	show Diamonds = "♦"
	show Clubs    = "♣"

instance Show Value where
	show value = [ "2","3","4","5","6","7","8","9","1","10","J","Q","K","A" ] !! fromEnum value

instance {-# OVERLAPPING #-} Show Card where
	show (value,suit) = show value ++ show suit

instance {-# OVERLAPPING #-} Read Card where
	readsPrec _ s = [ (card,"") | card <- deck, show card == s ]
