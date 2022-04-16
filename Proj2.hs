--------------------------------------------------------------------------------
-- Author: Archit Verma <architv@student.unimelb.edu.au>
-- Purpose: Card Guessing Game

-- This is a program for a card game between an answerer and a guesser.
-- The game is played with both players using a standard complete deck.
-- The answerer begins by selecting some cards from their deck.
-- The guesser tries to guess these cards by choosing the same number of
-- cards from their deck and showing it to the answerer. 
-- The answerer responds to the card by giving a feedback about them.
-- The feedback to the answer is represented using 5 numbers.
-- The numbers are:
--
-- 1. How many of the cards in the answer are also in the guess 
-- (correct cards).
--
-- 2. How many cards in the answer have rank lower than the lowest rank 
-- in the guess (lower ranks). Ranks, in order from low to high, are 2–10,
-- Jack, Queen, King, and Ace.
--
-- 3. How many of the cards in the answer have the same rank as a card in 
-- the guess (correct ranks). For this, each card in the guess is only 
-- counted once. That is, if the answer has two queens and the guess 
-- has one, the correct ranks number would be 1, not 2. Likewise if there
-- is one queen in the answer and two in the guess.
--
-- 4. How many cards in the answer have rank higher than the highest rank
-- in the guess (higher ranks).
--
-- 5. How many of the cards in the answer have the same suit as a card in 
-- the guess, only counting a card in the guess once (correct suits).
-- For example, if the answer has two clubs and the guess has one club, or
-- vice versa, the correct suits number would be 1, not 2.
--
-- For example:
-- Answer = [3Clubs, 4Hearts]
-- Guess = [4Hearts, 3Clubs]
-- Feedback = 2,0,2,0,2
--
-- Answer = [3Clubs, 4Hearts]
-- Guess = [3Clubs, 3Hearts]
-- Feedback = 1,0,1,1,2 
--
-- Answer = [3Diamonds, 3Spades]
-- Guess = [3Clubs, 3Hearts]
-- Feedback = 0,0,2,0,0
--
-- Answer = [3Clubs, 4Hearts]
-- Guess = [2Hearts, 3Hearts]
-- Feedback = 0,0,1,1,1
--
-- Answer = [AceClubs, 2Clubs]
-- Guess = [3Clubs, 4Hearts]
-- Feedback = 0,1,0,1,1
--
-- The guesser guesses again and receives a new feedback and the process 
-- continues until the guess is the correct answer.
-- The objective of the game is to find the correct answer in fewest number
-- of guesses
--------------------------------------------------------------------------------
-- Methodology
--
-- A Card module is provided which gives the Card, Rank and Suit types and 
-- their constructors.
--
-- The suits are (in increasing order) Club, Diamond, Heart, and Spade, and
-- the ranks are (in increasing order) R2, R3, R4, R5, R6, R7, R8, R9, 
-- R10, Jack, Queen, King, Ace, and cards are of the form Card suit rank.
--
-- Both the answerer and the guesser are implemented in this program.
-- A function is created to make an initial guess and another function to give
-- feedback which is used to determine the next guess through another function.
--
-- A GameState type is created which holds the information about the guesses
-- made and is used as an input to make new guesses.
-- 
-- Other helper function definition is provided in the code.
--------------------------------------------------------------------------------

module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- Creating a type GameState to hold state information. This includes
-- information about the guesses made.

type GameState = [[Card]]

-- The feedback function takes the answer and the guess as input and returns
-- a feedback in the same representation as mentioned above.
-- Each part of the 5 numbered feedback is calculated using the supporter 
-- functions which are described at their implementation.

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = (correctCards answer guess, lessGuess answer guess,
                        sameGuess answer guess, greaterGuess answer guess,
                        sameSuit answer guess)

-- correctCards functions takes the answer and guess as input and returns
-- the number of cards that are same. (Feedback 1)

correctCards :: [Card] -> [Card] -> Int
correctCards _[] = 0
correctCards x (y:ys)
    |(elem y x)    = 1 + (correctCards x ys)
    |otherwise     = (correctCards x ys)

-- lessGuess function takes the answer and the guess. It first finds the lowest
-- rank of a card in the guess and then returns the number of cards in the 
-- answer that has lower rank than the guess card with lowest rank. (Feedback 2)
-- It uses a supporter function onlyRank

lessGuess :: [Card] -> [Card] -> Int
lessGuess []_ = 0
lessGuess (x:xs) y
    |(rank x) < minimum (onlyRank y) = lessGuess xs y+1
    |otherwise = lessGuess xs y

-- sameGuess function takes the answer and the guess and returns the number of 
-- guess cards that have the same rank as the answer cards. (Feedback 3)
-- It uses a supporter function onlyRank

sameGuess :: [Card] -> [Card] -> Int
sameGuess answer guess = length ((onlyRank answer) \\ ((onlyRank answer)
                         \\ (onlyRank guess)))

-- greaterGuess function takes the answer and the guess. It first finds the 
-- highest rank of a card in the guess and then returns the number of cards 
-- in the answer that has higher rank than the guess card with highest rank. 
-- (Feedback 4)
-- It uses a supporter function onlyRank.

greaterGuess :: [Card] -> [Card] -> Int
greaterGuess []_ = 0
greaterGuess (x:xs) y
    |(rank x) > maximum (onlyRank y) = greaterGuess xs y+1
    |otherwise = greaterGuess xs y

-- onlyRank function is used to seperate the Rank from the suit rank
-- representation of cards.
-- This function is used as a supporter function for lessGuess, sameGuess and
-- greaterGuess Functions.

onlyRank :: [Card] -> [Rank]
onlyRank [] = []
onlyRank (x:xs) = (rank x) : (onlyRank xs)

-- sameSuit function takes the answer and the guess and returns the number of
-- guess cards that have the same suit as the answer cards. Each card is only 
-- used once. (Feedback 5)
-- It uses a supporter function onlySuit

sameSuit :: [Card] -> [Card] -> Int
sameSuit answer guess = length ((onlySuit answer) \\ ((onlySuit answer)
                        \\ (onlySuit guess)))

-- onlySuit function is used to seperate the Suit from the suit rank
-- representation of cards.
-- This function is used as a supporter function for sameSuit and is similar to
-- the onlyRank function mentioned above

onlySuit :: [Card] -> [Suit]
onlySuit [] = []
onlySuit (x:xs) = (suit x) : (onlySuit xs)

-- intialGuess function takes a number as input and returns the number of cards
-- as an intial guess along with a GameState.
-- The first guess is hardcoded becauase it can be any value. Based on the 
-- feedback only can we reach towards the answer. So the cards chosen for the 
-- intial guess does not matter as they can be anything.
-- It uses a supporter function makeGameState.

initialGuess :: Int -> ([Card],GameState)
initialGuess n
    |n == 2 = ([Card Club R4, Card Diamond R9], initialGameState)
    |otherwise = (head initialGameState, tail initialGameState)
    where initialGameState = makeGameState n

-- makeGameState function takes the number of cards and generate all possible
-- guesses.
-- It uses a supporter function permutationCards and duplicateFilter

makeGameState :: Int -> GameState
makeGameState n = filter duplicateFilter initialGameState
                  where initialGameState = sequence (permutationCards n)

-- permutationCards inputs the number of cards and returns all possible series
-- from that number of cards from a single deck. 

permutationCards :: Int -> [[Card]]
permutationCards 1 = [singleDeck]
permutationCards n = singleDeck:permutationCards (n-1)

-- singleDeck holds all the possible cards as a list.

singleDeck = [minBound..maxBound] :: [Card]

-- duplicateFilter returns a boolean value telling if the guess is unique and 
-- not repeated. 
-- True means unique.

duplicateFilter :: Eq a => [a] -> Bool
duplicateFilter [] = True
duplicateFilter (x:xs)
    |xs == []     = True
    |x == head xs = False
    |otherwise    = not(x `elem` xs) && duplicateFilter xs

-- nextGuess inputs the previous guess, gamestate and 5-numbered feedback and
-- returns a new pair of guess and gamestate. It also removes redundant guesses.

nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> ([Card], GameState)
nextGuess (initialGuess, gameState) lastFeedback = 
    ((head(filter(\xs->(feedback xs initialGuess) == lastFeedback) gameState))
    , (filter(\n->(feedback n initialGuess) == lastFeedback) gameState))

--------------------------------------------------------------------------------
---------------------------------END OF CODE------------------------------------