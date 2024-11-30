
module Poker where
    import System.Random
    import Data.List

    -- types 
    data Suit = Hearts | Spades | Diamonds | Clubs deriving (Show, Eq)
    data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King  deriving (Show, Eq)
    type Card = (Suit, Rank)
    type Deck = [Card]

    type PrivateHand = [Card]
    type Community = [Card]

    type Hand = (PrivateHand, Community)
    data Player = Player {name :: String, hand :: Hand, chip :: Int, isDealer :: Bool}

    {- shuffleDeck: Shuffle a deck of cards randomly -}
    shuffleDeck :: Deck -> IO Deck
