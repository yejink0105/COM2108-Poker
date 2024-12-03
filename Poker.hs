
module Poker where
    import System.Random
    import Data.List
    import Data.Ord (comparing)

    -- Step1
    -- types 
    data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord, Enum, Bounded)
    data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Show, Eq, Ord, Enum, Bounded)
    type Card = (Suit, Rank)
    type Deck = [Card]
    type Chip = Int

    --type Community = [Card]

    type Hand = [Card]
    data Player = Player {name :: String, hand :: Hand, chip :: Chip, isDealer :: Bool}

    data GameState = GameState {activePlayers :: [Player], deck :: Deck, communityCards :: [Card], 
    pot :: Chip, bets :: [Chip], dealer :: Int, sb :: Int, bb :: Int}

    {- createDeck: Create a deck in an order -}
    createdDeck :: Deck
    createdDeck = [(suit, rank) | suit <- suits, rank <- ranks]
        where
            suits = [minBound .. maxBound] :: [Suit]
            ranks = [minBound .. maxBound] :: [Rank]

    {- shuffleDeck: Shuffle a deck of cards randomly -}
    cmp :: (a, Int) -> (a, Int) -> Ordering
    cmp (_, y1) (_, y2) = compare y1 y2

    shuffleDeck :: [Card]
    shuffleDeck = [card | (card, _) <- sortBy cmp (zip createdDeck randomNumbers)]
        where
            randomNumbers = randoms (mkStdGen 1234) :: [Int]

    {- dealCards: Deal the hole and community cards-} --Player's hand is combined of community and private 
    dealCards :: Int -> Deck -> [Player] -> ([Player], Deck)
    dealCards _ deck [] = ([], deck)
    dealCards n deck (p:ps) =
        let (dealtHand, remainingDeck) = splitAt n deck  
            updatedPlayer = p { hand = dealtHand } -- max 7 cards        
            (updatedPlayers, finalDeck) = dealCards n remainingDeck ps 
        in (updatedPlayer : updatedPlayers, finalDeck)

    -- Step2
    -- types
    data PokerHand 
        = HighCard Rank
        | OnePair Rank
        | TwoPair Rank Rank
        | ThreeOfAKind Rank
        | Straight Rank
        | Flush [Card]
        | FullHouse Rank Rank
        | FourOfAKind Rank
        | StraightFlush Rank
        | RoyalFlush
        deriving (Show, Eq, Ord)

    evaluateHand :: Hand -> PokerHand 
    evaluateHand hand 
        | isRoyalFlush hand     = RoyalFlush
        | isStraightFlush hand  = StraightFlush (highestRank hand)
        | isFourOfAKind ranks   = FourOfAKind (head $ fourOfAKind ranks)
        | isFullHouse ranks     = FullHouse (head $ threeOfAKind ranks) (head $ twoOfAKind ranks)
        | isFlush suits         = Flush hand
        | isStraight ranks      = Straight (highestRank hand)
        | isThreeOfAKind ranks  = ThreeOfAKind (head $ threeOfAKind ranks)
        | isTwoPair ranks       = TwoPair firstPair secondPair
        | isOnePair ranks       = OnePair firstPair
        | otherwise             = HighCard (highestRank hand)
        where
            ranks = map snd hand
            suits = map fst hand
            firstPair = head (twoOfAKind ranks) --eg. Ace 
            secondPair = last (twoOfAKind ranks) 

    --Helper functions 
    isStraight :: [Rank] -> Bool
    isStraight ranks = sort ranks == [head ranks .. last ranks]

    isFlush :: [Suit] -> Bool
    isFlush suits = all (== head suits) suits

    isStraightFlush :: Hand -> Bool
    isStraightFlush hand = isStraight ranks && isFlush suits 
        where 
            ranks = map snd hand
            suits = map fst hand 

    isRoyalFlush :: Hand -> Bool
    isRoyalFlush hand = all (>= Ten) ranks && isFlush suits  
        where 
            ranks = map snd hand
            suits = map fst hand 

    --Helper functions 
    isOnePair :: [Rank] -> Bool
    isOnePair ranks = length (twoOfAKind ranks) == 1

    isTwoPair :: [Rank] -> Bool
    isTwoPair ranks = length (twoOfAKind ranks) == 2

    isFourOfAKind :: [Rank] -> Bool
    isFourOfAKind ranks = length (fourOfAKind ranks) == 1

    isThreeOfAKind :: [Rank] -> Bool
    isThreeOfAKind ranks = not (null $ threeOfAKind ranks)

    isFullHouse :: [Rank] -> Bool
    isFullHouse ranks = length (twoOfAKind ranks) == 1 && length (threeOfAKind ranks) == 1

    --Helper functions 
    highestRank :: Hand -> Rank
    highestRank hand = maximum (map snd hand)

    --Sort and group by rank into lists and filter lists which their length is 2
    twoOfAKind :: [Rank] -> [Rank] --eg. [Two, Ace]
    twoOfAKind = map head . filter (\x -> length x == 2) . group . sort

    threeOfAKind :: [Rank] -> [Rank]
    threeOfAKind = map head . filter (\x -> length x == 3) . group . sort

    fourOfAKind :: [Rank] -> [Rank]
    fourOfAKind = map head . filter (\x -> length x == 4) . group . sort -- takes only head from the sublist 

    {-determineWinner: determines the winner between multiple players based on their hand rankings-}
    determineWinner :: [Player] -> [Player]
    determineWinner players = 
        let playerHands = [(p, evaluateHand (hand p)) | p <- players]
            bestHand = maximumBy (comparing snd) playerHands
            winners = filter ((== snd bestHand) . snd) playerHands --get all players with equal best hands
        in map fst winners  

    -- Step3
    data Strategy = Bet | Call | Raise | Fold

    bettingRound :: GameState -> [Player] -> GameState -> [Player] --maybe GameState wouldn't be an input yet just input pot
    bettingRound = 
        --call initialising functions ??


    {-initialisePlayer: Initialises the Player-}
    initialisePlayer :: String -> Chip -> Player 
    initialisePlayer name chip = 
        let name' = name 
            hand' = []
            chip' = chip
            isDealer' = False
        in Player{name = name, hand = hand', chip = chip, isDealer = isDealer'}


    {-initialiseGameState: Initialise the GameSate-}
    initialiseGameState :: [Player] -> Int -> GameState 
    initialiseGameState players n =  -- rounds of game, 0-3
        let deck' = createdDeck
            communityCards' = []
            pot' = 0
            bets' = []
            dealer' = 0 --have to be decided randomly later 
            sb' = dealer' + 1
            bb' = dealer' + 2
        in GameState {activePlayers = players, deck = deck', communityCards = communityCards', pot = pot',
                    bets = bets', dealer = dealer', sb = sb', bb = bb'}

    main :: IO ()
    main = do
        let player1 = Player "Alice" [(Hearts, Ace), (Spades, Ace), (Diamonds, King), (Clubs, Queen), (Hearts, Ten)] 100 False
        let player2 = Player "Bob" [(Clubs, King), (Diamonds, King), (Spades, Queen), (Hearts, Jack), (Spades, Ten)] 100 False
        let player3 = Player "Charlie" [(Diamonds, Ten), (Clubs, Ten), (Hearts, Nine), (Spades, Eight), (Diamonds, Seven)] 100 False
        let players = [player1, player2, player3]

        putStrLn $ "Winner(s): " ++ show (map name (determineWinner players))



