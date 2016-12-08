--------------------------TicTacToe.hs----------------------------------
{-
Quinton Pryce
Tic Tac Toe
December 7, 2016
-}
------------------------------------------------------------------------

module TicTacToe (
Position,
Player (X,O), 
Game (Game), 
newGame,
getCell,
gameEnd, 
getCurrTurn,
showBoard, 
strSplit, 
makeMove
) where

import Data.List
import Data.Maybe
import Data.Map

type Position = (Int, Int)
type Board = [[Cell]] 
type Cell = Maybe Player
data Player = X | O deriving (Show, Eq)
data Game = Game { board :: Board, currentTurn :: Player} deriving Show

------------------------------------------------------------------------
--makes a 3x3 board of Nothing
------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 Nothing
------------------------------------------------------------------------
--makes new game, initalizes board and sets the player to go first
------------------------------------------------------------------------
newGame :: Game
newGame = Game { board = emptyBoard, currentTurn = X }
------------------------------------------------------------------------
--takes a position and a cell to play on the board
--splits the column at the correct row, removes the cell at position
------------------------------------------------------------------------
setCell :: Board -> Position -> Cell -> Board
setCell board pos cell = a2 ++ newRow : a4
  where
        (a1,_:a3) = splitAt (snd pos) (board !! (fst pos))
        newRow = a1 ++ cell : a3
        (a2,_:a4) = splitAt (fst pos) board    

getCell :: Game -> Position -> Cell
getCell (Game board _) pos = (board !! (fst pos) !! (snd pos))   
------------------------------------------------------------------------
--checks vertical, horizontal and diagonal (front back) wins
------------------------------------------------------------------------
winCheck :: Board -> Board
winCheck board = regularBoard ++ transBoard ++ [diagonalBoardf, diagonalBoardb]
  where regularBoard = board
        transBoard = transpose board
        diagonalBoardf = zipWith (\ x y -> x !! (3 - y - 1)) board [0..2]
        diagonalBoardb = zipWith (\ x y -> x !! ( y )) board [0..2]
------------------------------------------------------------------------
--checks the current board, if any of the returned values are all one player
--returns the winner
------------------------------------------------------------------------
gameEnd :: Game -> Cell
gameEnd (Game board _)
    | gameEnd' X  = Just X
    | gameEnd' O  = Just O
    | otherwise = Nothing
    where
        gameEnd' :: Player -> Bool
        gameEnd' player = any (all (== Just player)) $ winCheck board
------------------------------------------------------------------------
--gets the current turn from the game and returns "X" or "O"
------------------------------------------------------------------------
getCurrTurn :: Game -> String
getCurrTurn (Game b p) = player where
        player
            | p == X = "X"
            | p == O = "O"
            | otherwise = " "
------------------------------------------------------------------------
--returns player at position (Int, Int)
------------------------------------------------------------------------
showPlayer :: Board -> Int -> Int -> String
showPlayer board a b = player where
        player
            | board !! a !! b == Just X = " X "
            | board !! a !! b == Just O = " O "
            | otherwise = "   "
------------------------------------------------------------------------
--returns a string of the current board
------------------------------------------------------------------------
showBoard :: Game -> String
showBoard (Game board player) = list where
    list = showPlayer board 0 0 ++ "|" ++ showPlayer board 0 1 ++ "|" ++ showPlayer board 0 2 ++ "\n" ++
           "---+---+---\n" ++
           showPlayer board 1 0 ++ "|" ++ showPlayer board 1 1 ++ "|" ++ showPlayer board 1 2 ++ "\n" ++
           "---+---+---\n" ++
           showPlayer board 2 0 ++ "|" ++ showPlayer board 2 1 ++ "|" ++ showPlayer board 2 2 ++ "\n"
------------------------------------------------------------------------
--error checks for >3 string length (ie. not "move 1 2")
--takes a move command "move 1 2",splits it into a position (Int, Int)
------------------------------------------------------------------------
strSplit :: String -> Position
strSplit str = pos where
    pos
        | (length (words str) == 3) = validMove (words str)
        | otherwise = (3,3)

intafy :: [String] -> [Int]
intafy = Data.List.map read

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)
------------------------------------------------------------------------
--checks that a valid move position has been entered
------------------------------------------------------------------------
validMove :: [String] -> Position
validMove ["move", a, b] = pos where
        pos
            | ((a == "1"||a =="2"||a =="0") && (b == "1"||b =="2"||b =="0")) = tuplify (intafy [a, b])
            | otherwise = (3,3)
------------------------------------------------------------------------
--if the position is not already occupied, it makes the move
------------------------------------------------------------------------
makeMove :: Game -> Position -> Game
makeMove (Game board player) (x,y) = game where
    game
        | (isNothing $ board !! x !! y) = move (Game board player) (x,y)
        | otherwise = (Game board player)

move :: Game -> Position -> Game
move (Game board player) (x, y) = Game { board = newBoard, currentTurn = nextTurn } where
    newBoard = setCell board (x, y) (Just player) 
    nextTurn
        | player == X = O
        | otherwise = X
