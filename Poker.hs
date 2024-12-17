{-
    Module: Poker
    Description: this module defines data structure and functions used in Texas hold'Em Poker game
    Author: Yejin Kang 
    "No generative AI tools were used in the preparation of the solution to this work." 
-}


module Poker where
    import System.Random
    import Data.List
    import Data.Ord (comparing)
    import Control.Monad 


    -- Step1
    -- types 
    data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord, Enum, Bounded)
    data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Show, Eq, Ord, Enum, Bounded)
    type Card = (Suit, Rank)
    type Deck = [Card]
    newtype Chip = Chip Int deriving (Show, Eq, Ord)

    data PlayerType = RandomPlayer | AggressivePlayer | PassivePlayer | SmartPlayer deriving (Show, Eq)
    type Hand = [Card]
    data Player = Player {name :: String, hand :: Hand, chip :: Chip, 
    isDealer :: Bool, playerType :: PlayerType} deriving (Show, Eq)

    data GameState = GameState {activePlayers :: [Player], deck :: Deck, communityCards :: [Card], 
    pot :: Chip, bets :: [Chip], dealer :: Int, sb :: Int, bb :: Int, roundCount :: Int} deriving (Show, Eq)

    data Action = Bet Chip | Call | Raise Chip | Fold | Check deriving(Show, Eq)

    {- createDeck: Create a deck in an order -}
    createdDeck :: Deck
    createdDeck = [(suit, rank) | suit <- suits, rank <- ranks]
        where
            suits = [minBound .. maxBound] :: [Suit]
            ranks = [minBound .. maxBound] :: [Rank]

    {-cmp: comparing function to shuffle a deck-}
    cmp :: (a, Int) -> (a, Int) -> Ordering
    cmp (_, y1) (_, y2) = compare y1 y2

    {- shuffleDeck: Shuffle a deck of cards randomly -}
    shuffleDeck :: IO Deck 
    shuffleDeck = do
        gen <- newStdGen  
        let randomNumbers = randoms gen :: [Int]  
        return [card | (card, _) <- sortBy cmp (zip createdDeck randomNumbers)]

    {- dealCards: Deal the hole and community cards-} --Player's hand is combined with community and hole cards 
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
        | Flush [Rank]
        | FullHouse Rank Rank
        | FourOfAKind Rank
        | StraightFlush Rank
        | RoyalFlush
        deriving (Show, Eq, Ord)

    {-evaluateHand: evaluate hand as a poker hand-}
    evaluateHand :: Hand -> PokerHand 
    evaluateHand hand 
        | isRoyalFlush hand = RoyalFlush
        | isStraightFlush hand = StraightFlush (highestRank hand)
        | isFourOfAKind ranks = FourOfAKind (head $ fourOfAKind ranks)
        | isFullHouse ranks = FullHouse (head $ threeOfAKind ranks) (head $ twoOfAKind ranks)
        | isFlush suits = Flush (flushRanks hand)
        | isStraight ranks = Straight (highestRank hand)
        | isThreeOfAKind ranks = ThreeOfAKind (head $ threeOfAKind ranks)
        | isTwoPair ranks = TwoPair firstPair secondPair
        | isOnePair ranks = OnePair firstPair
        | otherwise = HighCard (highestRank hand)
        where
            ranks = map snd hand
            suits = map fst hand
            firstPair = head (twoOfAKind ranks) 
            secondPair = last (twoOfAKind ranks) 

    --Helper functions 
    isStraight :: [Rank] -> Bool
    isStraight ranks = 
        let sortedRanks = sort ranks
        in and $ zipWith (\a b -> succ a == b) sortedRanks (tail sortedRanks)

    isFlush :: [Suit] -> Bool
    isFlush suits = any (\suit -> length (filter (== suit) suits) >= 5) (nub suits)
    -- Return true if any combination of 5 suits have the same suit

    isStraightFlush :: Hand -> Bool
    isStraightFlush hand = isStraight ranks && isFlush suits 
        where 
            ranks = map snd hand
            suits = map fst hand 

    isRoyalFlush :: Hand -> Bool
    isRoyalFlush hand = isFlush suits && any (\rank -> length (filter (>= Ten) ranks) >=5) ranks 
        where 
            ranks = map snd hand
            suits = map fst hand
    -- Return true if it is flush and at least five ranks have higher ranks than Ten 

    isOnePair :: [Rank] -> Bool
    isOnePair ranks = length (twoOfAKind ranks) == 1
    -- Return true if there is only one two of a kind

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
    
    twoOfAKind :: [Rank] -> [Rank] 
    twoOfAKind = map head . filter (\x -> length x == 2) . group . sort
    --Sort and group by rank into lists and filter lists which their length is 2

    threeOfAKind :: [Rank] -> [Rank]
    threeOfAKind = map head . filter (\x -> length x == 3) . group . sort

    fourOfAKind :: [Rank] -> [Rank]
    fourOfAKind = map head . filter (\x -> length x == 4) . group . sort -- takes only head from the sublist 

    flushRanks :: Hand -> [Rank]
    flushRanks hand = 
        let suits = map fst hand
            dominantSuit = head $ head $ filter (\x -> length x >= 5) $ group $ sort suits --Select group of suits with its length more or equal to 5
            flushCards = filter (\(suit, _) -> suit == dominantSuit) hand -- filter cards with dominant suit 
        in 
            take 5 $ sortBy (flip compare) $ map snd flushCards -- Take 5 highest ranked cards from flushCards

    {-determineWinner: determines the winner between multiple players based on their hand rankings-}
    determineWinner :: [Player] -> [Player]
    determineWinner players = 
        let playerHands = [(p, evaluateHand (hand p)) | p <- players]
            bestHand = maximumBy (comparing snd) playerHands -- Compare PokerHands and find the best hand
            winners = filter ((== snd bestHand) . snd) playerHands -- Filter all tuples that are same with bestHand
        in if length winners > 1
            then tieBreaker winners -- when multiple players have the same Poker hand 
            else map fst winners

    {-tieBreaker: determines the winner by comparing the rank of the poker hand-}
    tieBreaker :: [(Player, PokerHand)] -> [Player]
    tieBreaker winners =
        let sortedByRank = sortBy (comparing (extractRank . snd)) winners
        in map fst sortedByRank

    {-extractRank: extract only ranks from the poker hand-}
    extractRank :: PokerHand -> [Rank]
    extractRank (HighCard r) = [r]
    extractRank (OnePair r) = [r]
    extractRank (TwoPair r1 r2) = [r1, r2]
    extractRank (ThreeOfAKind r) = [r]
    extractRank (Straight r) = [r]
    extractRank (Flush rs) = reverse rs  
    extractRank (FullHouse r1 r2) = [r1, r2]
    extractRank (FourOfAKind r) = [r]
    extractRank (StraightFlush r) = [r]
    extractRank RoyalFlush = [] -- RoyalFlush returns empty; always wins 
    

    -- Step3
    {-bettingStages: a list of betting stages in a round-}
    bettingStages :: [GameState -> IO GameState]
    bettingStages = [preFlop, flop, turn, river]

    {-preFlop: preFlop stage a betting round-}
    preFlop :: GameState -> IO GameState
    preFlop gameState = do
        putStrLn "---------------------------PreFlop starts---------------------------"
        originalDeck <- shuffleDeck
        let players = activePlayers gameState

        --Deal hole cards & intialise dealer position index
        (dealerPos, sbPos, bbPos) <- selectDealerSbBbPos players
        let (playersGivenCards, updatedDeck) = dealCards 2 originalDeck players
            updatedDeckGamaeState = gameState {activePlayers = playersGivenCards, 
            deck = updatedDeck, dealer = dealerPos, sb = sbPos, bb = bbPos}
        
        -- remove SB and BB and rearrange players in an order when calling bettingRound 
        let filteredPlayers = [p | (i, p) <- zip [0..] players, i /= sbPos, i /= bbPos]
            rearrangedPlayers = rearrangePlayers dealerPos filteredPlayers
            updateBettersGameState = updatedDeckGamaeState {activePlayers = rearrangedPlayers}
            
        -- add bet of sb and bb to bets and pot
        let updatePotAndBetsGameStateSb = updatePotAndBets updateBettersGameState (Chip 1)
            updatePotAndBetsGameStateBb = updatePotAndBets updatePotAndBetsGameStateSb (Chip 2)
            resetPlayersState = updatePotAndBetsGameStateBb {activePlayers = playersGivenCards}
        gameStateAfterPreFlop <- bettingRound resetPlayersState rearrangedPlayers

        let currentPlayers = activePlayers gameStateAfterPreFlop
        let currentBet = last (bets gameStateAfterPreFlop)
        let currentPot = pot gameStateAfterPreFlop
        putStrLn ("Active players: " ++ show (map name (activePlayers gameStateAfterPreFlop)))
        putStrLn ("Dealer position: " ++ show dealerPos)
        putStrLn ("SB position: " ++ show sbPos)
        putStrLn ("BB position: " ++ show bbPos)
        putStrLn("Bets:" ++ show (bets gameStateAfterPreFlop))
        putStrLn ("Current bet: " ++ show currentBet)
        putStrLn ("Current pot: " ++ show currentPot)

        return gameStateAfterPreFlop

    {-flop: Flop stage a betting round-}
    flop :: GameState -> IO GameState
    flop gameState = do 
        putStrLn "---------------------------Flop starts---------------------------"
        let currentDeck = deck gameState
            players = activePlayers gameState
            dealerPos = dealer gameState
            (playersGivenCards, updatedDeck) = dealCards 3 currentDeck players 
            dealtCommunityCards = take 3 currentDeck
            rearrangedPlayers = rearrangePlayers dealerPos playersGivenCards 
            updatedGameState = gameState {activePlayers = rearrangedPlayers, deck = updatedDeck, communityCards = dealtCommunityCards}
        gameStateAfterFlop <- bettingRound updatedGameState rearrangedPlayers

        let currentPlayers = activePlayers gameStateAfterFlop
        let currentPot = pot gameStateAfterFlop
        putStrLn ("Active players: " ++ show (map name (activePlayers gameStateAfterFlop)))
        putStrLn ("Community cards: " ++ show (communityCards gameStateAfterFlop))
        putStrLn ("Dealer position: " ++ show dealerPos)
        putStrLn("Bets:" ++ show (bets gameStateAfterFlop))
        putStrLn ("Current pot: " ++ show currentPot)
        return gameStateAfterFlop

    {-turn: Turn stage a betting round-}
    turn :: GameState -> IO GameState
    turn gameState = do
        putStrLn "---------------------------Turn starts---------------------------"
        let currentDeck = deck gameState
            players = activePlayers gameState
            dealerPos = dealer gameState
            (playersGivenCards, updatedDeck) = dealCards 1 currentDeck players 
            dealtCommunityCards = take 1 currentDeck ++ communityCards gameState
            rearrangedPlayers = rearrangePlayers dealerPos playersGivenCards
            updatedGameState = gameState {activePlayers = rearrangedPlayers, deck = updatedDeck, communityCards = dealtCommunityCards}
        gameStateAfterTurn <- bettingRound updatedGameState rearrangedPlayers

        let currentPlayers = activePlayers gameStateAfterTurn
        let currentPot = pot gameStateAfterTurn
        putStrLn ("Active players: " ++ show (map name (activePlayers gameStateAfterTurn)))
        putStrLn ("Community cards: " ++ show (communityCards gameStateAfterTurn))
        putStrLn ("Dealer position: " ++ show dealerPos)
        putStrLn("Bets:" ++ show (bets gameStateAfterTurn))
        putStrLn ("Current pot: " ++ show currentPot)

        return gameStateAfterTurn

    {-river: River stage a betting round-}
    river :: GameState -> IO GameState
    river gameState = do 
        putStrLn "---------------------------River starts---------------------------"
        let currentDeck = deck gameState
            players = activePlayers gameState
            dealerPos = dealer gameState
            (playersGivenCards, updatedDeck) = dealCards 1 currentDeck players 
            dealtCommunityCards = take 1 currentDeck ++ communityCards gameState
            rearrangedPlayers = rearrangePlayers dealerPos playersGivenCards
            updatedGameState = gameState {activePlayers = rearrangedPlayers, deck = updatedDeck, communityCards = dealtCommunityCards}
        gameStateAfterRiver <- bettingRound updatedGameState rearrangedPlayers

        let currentPlayers = activePlayers gameStateAfterRiver
        let currentPot = pot gameStateAfterRiver
        putStrLn ("Active players: " ++ show (map name (activePlayers gameStateAfterRiver)))
        putStrLn ("Community cards: " ++ show (communityCards gameStateAfterRiver))
        putStrLn ("Dealer position: " ++ show dealerPos)
        putStrLn("Bets:" ++ show (bets gameStateAfterRiver))
        putStrLn ("Current pot: " ++ show currentPot)
        return gameStateAfterRiver

    {-showdown: Show down stage -}
    showdown :: GameState -> IO ()
    showdown gameState = do 
        putStrLn "---------------------------Showdown starts---------------------------"
        let winners = determineWinner (activePlayers gameState)
        if length winners == 1 then do
            let winner = head winners
                winnerHand = evaluateHand (hand winner)
            putStrLn ("Game Over: the winner is " ++ name winner ++ " with " ++ show winnerHand)
        else if roundCount gameState >= 100 then do 
            let finalWinner = maximumBy (\p1 p2 -> compare (chip p1) (chip p2)) (activePlayers gameState)
            putStrLn ("Game Over: winner with the most chips after 100 rounds is: " ++ name finalWinner)
        else do
            putStrLn "It's a tie. Starting the next round:"
            let currentPot = pot gameState 
                newGameState = initialiseGameState winners
                updatePotGameState = newGameState {pot = currentPot} 
            gameLoop updatePotGameState bettingStages 

    {--rearrangePlayers: rearrange the order of players based on the dealer position-}
    rearrangePlayers :: Int -> [Player] -> [Player]
    rearrangePlayers position players = 
        let firstPlayerPos = (position + 1) `mod` length players -- first player position is next to the dealer position
            (left, right) = splitAt firstPlayerPos players 
        in right ++ left -- put players in ealier orders than first player to the back of the list
            
    {-selectDealerSbBbPos: select dealer position randomly and set sb and bb positions accordingly-}
    selectDealerSbBbPos :: [Player] -> IO (Int, Int, Int)
    selectDealerSbBbPos players = do
        let max = length players - 1
        dealerPos <- randomRIO (0, max) -- randomly selects delaer position
        let sbPos = (dealerPos + 1) `mod` length players -- next to the dealer
            bbPos = (dealerPos + 2) `mod` length players -- next to the sb
        return (dealerPos, sbPos, bbPos)

    {-endGame: terminate the game by printing a message-}
    endGame :: GameState -> IO ()
    endGame gameState = do
        let winner = name (head (activePlayers gameState))
        putStrLn ("Game Over: the winner is " ++ show winner)


    {-gameLoop: executes betting stages in a round-}
    gameLoop :: GameState -> [GameState -> IO GameState] -> IO ()
    gameLoop gameState [] = gameLoop gameState bettingStages
    gameLoop gameState (stage:remainingStages) 
        | length (activePlayers gameState) <= 1 = endGame gameState --end game if active players are 1 or less
        | otherwise = do
            updatedState <- stage gameState
            if null remainingStages && length (activePlayers updatedState) > 1 
            then showdown updatedState -- call showdown if remaining betting stages are exausted and active players are more than one
            else gameLoop updatedState remainingStages

    
    {-bettingRound: process players' action during a betting stage-}
    bettingRound :: GameState -> [Player] -> IO GameState
    bettingRound gameState [] = return gameState  
    bettingRound gameState (player:players) = do
        let currentBet = if null (bets gameState) then Chip 0 else last (bets gameState)
        action <- selectStrategy player currentBet
        updatedGameState <- processAction gameState player action currentBet
        if isEnd (activePlayers updatedGameState)
            then do
                return updatedGameState -- Stop  
            else bettingRound updatedGameState players -- Continue to choose action and process it

    {-isEnd: return true if terminating condition has met-}
    isEnd :: [Player] -> Bool
    isEnd players 
        | length players <= 1 = True 
        | otherwise = False


    {-selectStrategy: select and call player's strategy based on user type-}
    selectStrategy :: Player -> Chip -> IO Action
    selectStrategy player currentBet = case playerType player of
        RandomPlayer -> randomPlayerStrategy player currentBet
        PassivePlayer -> passivePlayerStrategy player currentBet
        AggressivePlayer -> aggressivePlayerStrategy player currentBet 
        -- more players to be continued...

    {-randomPlayerStrategy: determine available actions based on the amount of player's chip and betting stage 
    and choose one randomly-}
    randomPlayerStrategy :: Player -> Chip -> IO Action     
    randomPlayerStrategy player (Chip currentBet) = do
        let (Chip currentChip) = chip player
        betAmount <- randomRIO (0, currentChip)
        raiseAmount <- randomRIO (currentBet + 1, currentChip)
        let possibleActions
                | currentChip < currentBet = [Fold]
                | length (hand player) == 2 && currentChip == currentBet = [Fold, Call]
                | length (hand player) == 2 = [Fold, Call, Raise (Chip raiseAmount)] -- During pre-flop
                | currentBet == 0 && currentChip /= 0 = [Fold, Bet (Chip betAmount), Check]
                | currentBet == 0 && currentChip == 0 = [Check, Fold]
                | currentChip == currentBet = [Fold, Call]
                | otherwise = [Fold, Call, Raise (Chip raiseAmount)]
        randomIndex <- randomRIO (0, length possibleActions - 1)
        return (possibleActions !! randomIndex)

    {-passivePlayerStrategy: determine available actions which are limited to Check and Fold and randomly choose one-}
    passivePlayerStrategy :: Player -> Chip -> IO Action 
    passivePlayerStrategy player (Chip currentBet) = do
        let (Chip currentChip) = chip player
            possibleActions
                | currentBet == 0 = [Check, Fold]
                | currentChip < currentBet = [Fold]
                | otherwise = [Fold, Call]
        randomIndex <- randomRIO (0, length possibleActions - 1)
        return (possibleActions !! randomIndex)

    {-aggressivePlayerStrategy: determine actions with a higher possibility to bet and raise -}
    aggressivePlayerStrategy :: Player -> Chip -> IO Action
    aggressivePlayerStrategy player (Chip currentBet) = do
        let (Chip currentChip) = chip player
        betAmount <- randomRIO (0, currentChip)
        raiseAmount <- randomRIO (currentBet + 1, currentChip)
        let possibleActions 
                | currentChip < currentBet = [Fold]
                | length (hand player) == 2 && currentChip == currentBet = [Fold, Call]
                | length (hand player) == 2 = [Fold, Call, Raise (Chip raiseAmount), 
                Raise (Chip raiseAmount), Raise (Chip raiseAmount)] -- Raise by 60% of possibility
                | currentBet == 0 && currentChip /= 0 = [Fold, Bet (Chip betAmount), 
                Bet (Chip betAmount), Bet (Chip betAmount), Check] -- Bet by 60% of possibility 
                | currentBet == 0 && currentChip == 0 = [Check, Fold]
                | currentChip == currentBet = [Fold, Call]
                | otherwise = [Fold, Call, Raise (Chip raiseAmount), Raise (Chip raiseAmount), 
                Raise (Chip raiseAmount)] -- Raise by 60% of possibility
        randomIndex <- randomRIO (0, length possibleActions - 1)
        return (possibleActions !! randomIndex)


    {-processAction: process action by updating the player and game state and printing a message-}
    processAction :: GameState -> Player -> Action -> Chip -> IO GameState
    processAction gameState player (Bet (Chip amount)) _ = do
        let updatedPlayer = player {chip = Chip (currentChip - amount)}
            updatedPotGameState = updatePotAndBets gameState (Chip amount)
            updatedPlayerGameState = updatePlayer updatedPotGameState updatedPlayer
        putStrLn $ show (name player) ++ " has bet " ++ show amount
        return updatedPlayerGameState
            where 
                Chip currentChip = chip player
    
    processAction gameState player Call (Chip currentBet) = do
        let updatedPlayer = player {chip = Chip (currentChip - currentBet)}
            updatedPotGameState = updatePotAndBets gameState (Chip currentBet)
            updatedPlayerGameState = updatePlayer updatedPotGameState updatedPlayer
        putStrLn $ show (name player) ++ " has call"
        return updatedPlayerGameState
            where 
                Chip currentChip = chip player

    processAction gameState player (Raise (Chip amount)) _ = do
        let updatedPlayer = player {chip = Chip (currentChip - amount)}
            updatedPotGameState = updatePotAndBets gameState (Chip amount)
            updatedPlayerGameState = updatePlayer updatedPotGameState updatedPlayer
        putStrLn $ show (name player) ++ " has raise to " ++ show amount
        return updatedPlayerGameState
            where 
                Chip currentChip = chip player

    processAction gameState player Fold _ = do
        putStrLn $ show (name player) ++ " has fold, therefore eliminated from this round" 
        let updatedGameState = gameState {activePlayers = filter (/= player) (activePlayers gameState)}
        return updatedGameState
    
    processAction gameState player Check _ = do
        putStrLn $ show (name player) ++ " has check"
        return gameState

    {-updatePotAndBets: update pot amount and bets in game state-}
    updatePotAndBets :: GameState -> Chip -> GameState
    updatePotAndBets gameState (Chip amount) = gameState {pot = Chip (currentPot + amount), 
    bets = bets gameState ++ [Chip amount]}
        where 
            Chip currentPot = pot gameState

    {-updatePlayer: removes the player with same name with updatedPlayer activePlayers and add updatedPlayer to the list-}
    updatePlayer :: GameState -> Player -> GameState
    updatePlayer gameState updatedPlayer =
        gameState {activePlayers = filter ((/= name updatedPlayer) . name) (activePlayers gameState) ++ [updatedPlayer]}

    
    {-initialisePlayer: Initialises the Player-}
    initialisePlayer :: String -> Chip -> Player 
    initialisePlayer name chip = 
        let name' = name 
            hand' = []
            chip' = chip
            isDealer' = False
            playerType' = RandomPlayer -- Should I decided this randomly?
        in Player{name = name, hand = hand', chip = chip, isDealer = isDealer', playerType = playerType'}


    {-initialiseGameState: Initialise the GameSate-}
    initialiseGameState :: [Player] ->  GameState
    initialiseGameState activePlayers =  
        let deck' = []
            communityCards' = []
            pot' = Chip 0
            bets' = []
            dealer' = 0  
            sb' = dealer' + 1
            bb' = dealer' + 2
            roundCount' = 1
        in GameState {activePlayers = activePlayers, deck = deck', communityCards = communityCards', pot = pot',
                    bets = bets', dealer = dealer', sb = sb', bb = bb', roundCount = roundCount'}

    main :: IO ()
    main = do
        let player1 = Player "Alice" [] (Chip 1000) False RandomPlayer
        let player2 = Player "Bob" [] (Chip 1000) False PassivePlayer
        let player3 = Player "Charlie" [] (Chip 1000) False RandomPlayer
        let player4 = Player "Nina" [] (Chip 1000) False AggressivePlayer
        let player5 = Player "Jack" [] (Chip 1000) False AggressivePlayer
        let players = [player1, player2, player3, player4, player5]

        let gameState = initialiseGameState players 
        putStrLn "Game Starts"
        gameLoop gameState bettingStages



















