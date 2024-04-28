module Main where

import Data.IORef
import Data.Char (ord)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_, unless, when)

-- Convert a number to its corresponding letter
intToLetter :: Int -> Char
intToLetter n = toEnum (n + fromEnum 'a' - 1)

-- Convert a letter to its corresponding integer value
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a' + 1

-- Print a single row of the grid
printRow :: Int -> String
printRow n = intToLetter n : ' ' : concatMap (\_ -> "- ") [1..10]

-- Print the entire game board
printGameboard :: IO ()
printGameboard = do 
    putStrLn "------GAME BOARD-----"
    mapM_ putStrLn [printRow n | n <- [1..10]]
    putStrLn "  0 1 2 3 4 5 6 7 8 9"

-- Function to add an element to the list at index i in the list coordinatesP1
addElementAtIndex :: Int -> String -> IORef [[String]] -> IO ()
addElementAtIndex i element listRef = do
  -- Read the current value of coordinatesP1
  currentList <- readIORef listRef
  -- Check if the index i is within bounds
  if i >= 0 && i < length currentList
    then do
      -- Append the new element to the list at index i
      let updatedList = currentList !! i ++ [element]
      -- Write the updated list back to coordinatesP1
      writeIORef listRef $ take i currentList ++ [updatedList] ++ drop (i+1) currentList
    else
      putStrLn "Invalid index for adding element"

-- Define a list of ship lengths
shipLengths :: [Int]
shipLengths = [2, 3, 4, 5]

-- Function to remove an element from a list if it exists and return the modified list
removeIfExists :: Eq a => a -> [a] -> [a]
removeIfExists _ [] = []
removeIfExists x (y:ys)
    | x == y && null ys = []
    | x == y = ys
    | otherwise = y : removeIfExists x ys

-- Function to print "hit" if the element exists in any sublist of the list and "miss" if it doesn't
printHitOrMiss :: Eq a => a -> [[a]] -> IO ()
printHitOrMiss _ [] = putStrLn "Miss!"  -- If the list is empty, print "miss"
printHitOrMiss x (ys:yss)
    | x `elem` ys = do
        if length ys == 1
            -- If the length of the sublist is 1, print "hit and sunk"
            then putStrLn "Hit and sunk!"
            -- Otherwise, just print "hit"
            else putStrLn "Hit!"
     -- Recur on the remaining list
    | otherwise = printHitOrMiss x yss

-- Two-dimensional list to store player 1 ship coordinates
coordinatesP1 :: IORef [[String]]
coordinatesP1 = unsafePerformIO $ newIORef [[], [], [], []]

-- Two-dimensional list to store player 2 ship coordinates
coordinatesP2 :: IORef [[String]]
coordinatesP2 = unsafePerformIO $ newIORef [[], [], [], []]

-- Function to validate input
isValidInput :: String -> Bool
isValidInput input =
    length input == 2 &&
    head input `elem` ['a'..'j'] &&
    last input `elem` ['0'..'9']

-- Function to ask for input until it is valid
askForValidShot :: IORef [[String]] -> String -> IO String
askForValidShot coordinatesList playerName = do
    putStrLn $ "Player " ++ playerName ++ ", enter your shooting coordinate (e.g., 'a1'): "
    input <- getLine
    if isValidInput input
        then do
            listValue <- readIORef coordinatesList
            printHitOrMiss input listValue
            modifiedList <- fmap (removeIfExists input) <$> readIORef coordinatesList
            -- Update the coordinatesList with the modified list
            writeIORef coordinatesList modifiedList
            return input
        else do
            putStrLn "Invalid input! Please enter a string starting with a letter between 'a' and 'j' and ending with a number between '0' and '9'."
            askForValidShot coordinatesList playerName



-- Ask to place the ships
askShipPlaces :: String -> IORef [[String]] -> IO ()
askShipPlaces playerName coordinatesList = do
    putStrLn $ playerName ++ ", place your ships!"
    forM_ shipLengths $ \i -> do 
        -- function to iterate through all the ship lengths
        putStrLn $ "Enter row or column for your ship length of " ++ show i ++ ": "
        shipidentifier <- getLine
        if shipidentifier `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
          then do
            putStrLn "Enter starting row for your ship: "
            shipstart <- getLine
            if length shipstart == 1 && head shipstart `elem` ['a'..'j']
              then do
                if letterToInt (head shipstart) + i <= 10
                  then do
                    let initCoordinate = shipstart ++ shipidentifier
                    -- Update the given coordinates list with the new ship's initial coordinate
                    addElementAtIndex (i-2) initCoordinate coordinatesList
                    -- Iterate over the remaining coordinates for the ship
                    forM_ [1..(i-1)] $ \coordinate -> do
                      let newCoordinate = [intToLetter (letterToInt (head shipstart) + coordinate)] ++ shipidentifier
                      -- Update the given coordinates list with the new ship's additional coordinates
                      addElementAtIndex (i-2) newCoordinate coordinatesList
                else putStrLn "Invalid starting position! Please enter a valid starting position."
            else putStrLn "Enter a letter from 'a' to 'j'"
        else if shipidentifier `elem` ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
          then do
            putStrLn "Enter starting column for your ship: "
            shipstart <- getLine
            if length shipstart == 1 && head shipstart `elem` ['0' .. '9']
              then do
                if (read shipstart :: Int) + i <= 10
                  then do
                    let initCoordinate = shipidentifier ++ shipstart
                    -- Update the given coordinates list with the new ship's initial coordinate
                    addElementAtIndex (i-2) initCoordinate coordinatesList
                    -- Iterate over the remaining coordinates for the ship
                    forM_ [1..(i-1)] $ \coordinate -> do
                      let newCoordinate = shipidentifier ++ show (read shipstart + coordinate)
                      -- Update the given coordinates list with the new ship's additional coordinates
                      addElementAtIndex (i-2) newCoordinate coordinatesList
                else putStrLn "Invalid starting position! Please enter a valid starting position."
              else putStrLn "Enter a number from '0' to '9'"
        else putStrLn "Invalid input! Please enter a number between 1 and 10 or a letter between 'a' and 'j'."

-- Function to print each list in coordinatesP1 with its index
printCoordinates :: IORef [[String]] -> IO ()
printCoordinates coordinatesList = do
    -- Read the current value of the coordinates list
    coords <- readIORef coordinatesList
    -- Print each sublist of the coordinates list with its index
    mapM_ (\(idx, sublist) -> putStrLn $ "Index " ++ show idx ++ ": " ++ show sublist) (zip [0..] coords)

-- Function to check if all lists are empty
allListsEmpty :: IORef [[String]] -> IO Bool
allListsEmpty list = do
    coords <- readIORef list
    return (all null coords)

-- Function to play the game until one of the lists is empty
playGame :: IO ()
playGame = do
    empty1 <- allListsEmpty coordinatesP1
    empty2 <- allListsEmpty coordinatesP2
    unless (empty1 || empty2) $ do
        putStrLn "Player 1's turn:"
        _ <- askForValidShot coordinatesP2 "1"
        empty2 <- allListsEmpty coordinatesP2
        unless empty2 $ do
            putStrLn "Player 2's turn:"
            _ <- askForValidShot coordinatesP1 "2"
            empty1 <- allListsEmpty coordinatesP1

            unless empty1 $ playGame
            -- If Player 1's list is empty, Player 2 won
            when empty1 $ putStrLn "Player 2 won!"
        -- If Player 2's list is empty, Player 1 won
        when empty2 $ putStrLn "Player 1 won!"

-- Main function
main :: IO ()
main = do
    printGameboard
    askShipPlaces "Player 1" coordinatesP1
    askShipPlaces "Player 2" coordinatesP2
    playGame