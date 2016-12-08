-------------------------------irc.hs----------------------------------
{-
Quinton Pryce
IRC IO implementation for Tic Tac Toe
December 7, 2016
-}
------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import TicTacToe
import Network.SimpleIRC
import Data.Maybe

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar

data State = State {game :: Game}
type MState = MVar State

data IRCState = IRCState {mirc :: MIrc}
type IRCMState = MVar IRCState

instructions =  "New TicTacToe game made.\n" ++
                "Type \"show\" to see the board state.\n" ++
                "Type \"move\" followed by one of the board positions below.\n" ++
                "(ie. \"move 0 0\")\n" ++
                "0 0|0 1|0 2\n" ++
                "---+---+---\n" ++
                "1 0|1 1|1 2\n" ++
                "---+---+---\n" ++
                "2 0|2 1|2 2\n" 

------------------------------------------------------------------------
--gets the current game state from the saved MVar
------------------------------------------------------------------------
getGame :: MState -> IO (State)
getGame g = do
  temp <- readMVar g
  return $ temp
------------------------------------------------------------------------
--sets the game state to be saved by MVar
------------------------------------------------------------------------
setGame :: MState -> Game -> IO ()
setGame g newG = do
  modifyMVar_ g (\i -> return i {game = newG})
------------------------------------------------------------------------
--Data.Either.Unwrap
------------------------------------------------------------------------
fromRight (Right x) = x
------------------------------------------------------------------------
--takes client messages and processes them
------------------------------------------------------------------------
onClientMessage :: MState -> String -> MIrc -> B.ByteString -> IO ()
onClientMessage state line mIRC chan
  | line == "newgame" = do
    sendMsg mIRC chan (B.pack ("newgame"))
    setGame state (newGame)
    putStr instructions
    sendMsg mIRC chan (B.pack ("Opponent made new game:"))
    h <- getGame state
    sendMsg mIRC chan (B.pack (showBoard $ game h))
  | ((words line) !! 0 == "move") = do
    g <- getGame state
    if ((strSplit line) == (3,3)) then do
      putStrLn "Bad Input"
    else do
      if (getCell (game g) (strSplit line) == Nothing) then do
        sendMsg mIRC chan (B.pack (line))
        pos <- return $ ((strSplit (line)) :: Position)
        setGame state (makeMove (game g) pos)
        h <- getGame state
        putStr (showBoard $ game h)    
        if gameEnd (game h) == Just X then do
          putStrLn "Player X wins"
          sendMsg mIRC chan (B.pack (showBoard $ game h))
          sendMsg mIRC chan (B.pack ("Player O wins"))
        else return ()
        if gameEnd (game h) == Just O then do
          putStrLn "Player O wins"
          sendMsg mIRC chan (B.pack (showBoard $ game h))
          sendMsg mIRC chan (B.pack ("Player O wins"))
        else return ()    
        if gameEnd (game h) == Nothing then do 
          putStrLn ("Player " ++ getCurrTurn (game h) ++ "'s Turn")
          sendMsg mIRC chan (B.pack ("Opponent made a move:"))
          sendMsg mIRC chan (B.pack (showBoard $ game h))
          sendMsg mIRC chan (B.pack ("Player " ++ getCurrTurn (game h) ++ "'s Turn"))
        else return ()
      else putStrLn "Spot Occupied"
    | line == "show" = do
      h <- getGame state
      putStr (showBoard $ game h)
      putStrLn ("Player " ++ getCurrTurn (game h) ++ "'s Turn")
  | otherwise = do
    putStrLn "Not Valid Input"
------------------------------------------------------------------------
--takes input from IRC and processes the message
--EventFunc :: MIrc -> IrcMessage -> IO ()
------------------------------------------------------------------------
onIRCMessage :: MState -> EventFunc 
onIRCMessage state mIRC message
  | (msg == "newgame") = do
    setGame state (newGame)
    sendMsg mIRC chan (B.pack (instructions))
    putStrLn "Opponent made new game:"
    h <- getGame state
    putStr (showBoard $ game h)
  | B.isPrefixOf "move" msg = do
      g <- getGame state
      if (strSplit (B.unpack msg)) == (3,3) then do
        sendMsg mIRC chan (B.pack ("Bad Input"))
      else do
        if (getCell (game g) (strSplit (B.unpack msg)) == Nothing) then do
          pos <- return $ ((strSplit (B.unpack msg)) :: Position)
          g <- getGame state
          setGame state (makeMove (game g) pos)   
          sendMsg mIRC chan (B.pack ("Move Processing"))
          h <- getGame state
          sendMsg mIRC chan (B.pack (showBoard $ game h))
          if gameEnd (game h) == Just X then do
            sendMsg mIRC chan (B.pack ("Player X wins"))
            putStr (showBoard $ game h)
            putStrLn "Player X wins"
          else return ()
          if gameEnd (game h) == Just O then do
            sendMsg mIRC chan (B.pack ("Player O wins"))
            putStr (showBoard $ game h)
            putStrLn "Player O wins"
          else return ()    
          if gameEnd (game h) == Nothing then do 
            sendMsg mIRC chan (B.pack ("Player " ++ getCurrTurn (game h) ++ "'s Turn"))
            putStrLn ("Opponent made a move:")
            putStr (showBoard $ game h)
            putStrLn ("Player " ++ getCurrTurn (game h) ++ "'s Turn")
          else return ()
        else sendMsg mIRC chan (B.pack ("Spot Occupied"))
  | msg == "show" = do
    g <- getGame state
    sendMsg mIRC chan (B.pack ((showBoard $ game g)))
  | otherwise = do
    return ()
  where chan = fromJust $ mChan message
        msg = mMsg message
------------------------------------------------------------------------
--lookes for input from the console
------------------------------------------------------------------------
gameLoop state mirc chan = do
  line <- getLine
  onClientMessage state line mirc chan
  gameLoop state mirc chan

main = do
  state <- newMVar $ State newGame
  let events = [(Privmsg (onIRCMessage state))]
  
  let config = (mkDefaultConfig "irc.freenode.net" "QuintonBOT"){ cChannels = ["#meta"], cEvents = events}

  h <- connect config True False --MultiThread | Debug
  let mirc = fromRight h
  chans <- (getChannels mirc) -- gets the channel connected to (getChannels -> [B.ByteString])
  let chan = chans !! 0
  putStr instructions
  gameLoop state mirc chan
