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
	show value = [ "2","3","4","5","6","7","8","9","10","J","Q","K","A" ] !! fromEnum value

instance {-# OVERLAPPING #-} Show Card where
	show (value,suit) = show value ++ show suit

instance {-# OVERLAPPING #-} Read Card where
	readsPrec _ s = [ (card,"") | card <- deck, show card == s ]

--threeOfAKind hand = 

mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort left) (mergesort right)
	where
	middle_index = div (length l) 2
	left = take middle_index l
	right = drop middle_index l
	
	merge (first1:rest1) (first2:rest2) = case first1 <= first2 of
		True  -> first1 : merge rest1 (first2:rest2)
		False -> first2 : merge (first1:rest1) rest2
	merge [] l2 = l2
	merge l1 [] = l1

bubblesort l = bubblepass (length l) l
	where
	bubblepass 0 l = l
	bubblepass i l = bubblepass (i-1) (bubble l)

	bubble [] = []
	bubble [x] = [x]
	bubble (x1:x2:rest) = if x1<=x2 then x1 : bubble (x2:rest) else x2 : bubble (x1:rest)