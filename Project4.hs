{- Name: Zanyar Sherwani
    G#: G00841632

-}

module Project4 where

import Control.Monad  -- many useful functions
import Control.Concurrent -- threadDelay, forkIO, MVar..., Chan...
import Data.IORef -- newIORef, readIORef, writeIORef
import System.Environment -- getArgs

import System.Random -- randomRIO
import Debug.Trace

import Control.Concurrent.Chan





-- grabs n things from the BoundedChan and prints them in received order.
--announcer _ :: Int -> BC.BoundedChan String -> IO ()
announcer 0 _ = return ()
announcer n mVar = do
  v <- takeMVar mVar
  putStrLn $ v
  announcer (n-1) mVar


-- this function determines how many threads will be created
-- by reading in the command line arguments
numThreads:: IO(Int)
numThreads = do
  args <- getArgs
  let num_threads = if((length args) > 0)
                       then read (head args)::Int
                       else (10::Int)
  return num_threads

-- this function continues reading until the game has been stopped
waitForGame game = do
  g <- readMVar game
  if g then do waitForGame game else return ()

-- the base case is if there are no chairs left
emcee music round [] winLst loseLst game finalStr= do
  x <- takeMVar game
  putMVar game False
  t <- takeMVar finalStr
  putMVar winLst $ t -- print final winner

-- this is the function that helps coordinate when to start and stop the threads by
-- by turning the music on, it is also responsible for printing at the start and in
-- between rounds
emcee music round (x:xs) winLst loseLst game finalStr= do
  putMVar winLst $ "\nRound "++show round
  putMVar winLst $ "Music Off"
  t <- takeMVar music
  threadDelay 1000
  putMVar music True
  threadDelay 1000
  a <- takeMVar loseLst
  putMVar winLst $ a
  c <- takeMVar x
  putMVar x False
  resetChairs xs
  -- recursive call for next round
  emcee music (round+1) xs winLst loseLst game finalStr


-- main function
main :: IO ()
main = do
  n_threads <- numThreads
  winLst <- newEmptyMVar
  loseLst <- newEmptyMVar
  music <- newMVar False
  game <- newMVar True
  finalStr <- newEmptyMVar
  putStrLn $ "BEGIN " ++show n_threads++" players"
  chairs <- generateChairs (n_threads - 1)

  t <- newMVar True
  let r = (n_threads::Int)
  -- passes in a list of ints, representing players
  playerStart [p | p <- [1..n_threads] ] music chairs winLst loseLst n_threads game finalStr n_threads
  let t = foldr (+) 0 [0..r]
  announcer (t+(r-1)*2) winLst
  putStrLn $ "END"

-- a circular lock that basically keeps calling itself until music is turned on
waitForMusic music = do
  m <- readMVar music
  if(m == True) then do return () else do waitForMusic music


-- thing function recursively calls itself until it is down to the last
-- player in the list, each time forking a new thread
playerStart (pN:[]) music chairs winLst loseLst n_threads game finalStr r = do
  t <- newMVar True
  forkIO $ playerStartHelper t pN music chairs winLst loseLst r finalStr
  -- forks into the emcee function once all player threads have been created
  forkIO $ emcee music n_threads chairs winLst loseLst game finalStr
  return ()
playerStart (pN:playerTail) music chairs winLst loseLst n_threads game finalStr r=
  do
    t <- newMVar True

    forkIO  $ playerStartHelper t pN music chairs winLst loseLst r finalStr

    playerStart playerTail music chairs winLst loseLst (n_threads-1) game finalStr r
    return ()



playerStartHelper active player_num music chairs winLst lostLst (-1) finalStr= return ()
-- base case is when the rounds are over, then the last player has won
playerStartHelper active player_num music chairs winLst lostLst 1 finalStr= do
  putMVar finalStr $ "\nP"++show player_num++" wins!"
  return ()

playerStartHelper active player_num music chairs winLst loseLst round finalStr=do
  waitForMusic music

  a <- readMVar active

  if(a == True) then do
                      threadDelay 1000
                      b <-(sitInChair music player_num chairs chairs 1 winLst loseLst)
                      m <- takeMVar music
                      putMVar music False
                      (playerStartHelper b player_num music (tail chairs) winLst loseLst (round-1) finalStr)

  else do
    (playerStartHelper active player_num music (tail chairs) winLst loseLst (0-1) finalStr)


-- sets all chairs to true after each round
resetChairs (x:xs) = do
                      t <- takeMVar x
                      putMVar x True
                      resetChairs xs
resetChairs [] = return ()



-- if a player sits in a chair then it returns true
-- base case is an empty list of chairs meaning the player
-- never sat
sitInChair music pNum [] chairs chairNum winLst loseLst = do
                                        f <- newMVar False
                                        --threadDelay 400
                                        putMVar loseLst $ "P"++show pNum++" lost :("
                                        return f
sitInChair music pNum (x:xs) chairs chairNum winLst loseLst = do

                                            chair <- takeMVar x
                                            if(chair == True)
                                              then do
                                                putMVar x False
                                                putMVar winLst $ "P"++show pNum++" sat in C"++show chairNum

                                                t <- newMVar True
                                                return t
                                            else
                                              do
                                                putMVar x False
                                                sitInChair music pNum xs chairs (chairNum+1) winLst loseLst



generateChairs :: Int ->IO [MVar Bool]
generateChairs 0 = return []
generateChairs n = do
  chair <- (newMVar True)
  chairs <- generateChairs (n-1)
  return (chair:chairs)
  -- n > 0 =







