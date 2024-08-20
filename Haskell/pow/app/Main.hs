{-# LANGUAGE GADTs #-}

module Main where

import Data.Number.CReal

display :: Int -> Rational -> String
display digits num = showCReal digits (fromRational num)

main :: IO ()

-- define a Server type to determine whether it is Alice or Bob to serve
-- server should implement the Eq class
data Server where
  Alice :: Server
  Bob :: Server
  deriving (Eq)

-- expectedWinforAliceSwitchServe function to calculate the expected win probaility for Alice given the current server and the games required to win for Alice and Bob
expectedWinforAliceSwitchServe :: Server -> Int -> Int -> Rational
expectedWinforAliceSwitchServe _ 0 _ = 1
expectedWinforAliceSwitchServe _ _ 0 = 0
expectedWinforAliceSwitchServe Alice gamesToWinAlice gamesToWinBob
  | gamesToWinAlice < 0 || gamesToWinBob < 0 = error "Games to win should be non-negative"
  | otherwise =
      (3 / 4) * expectedWinforAliceSwitchServe Bob (gamesToWinAlice - 1) gamesToWinBob
        + (1 / 4) * expectedWinforAliceSwitchServe Bob gamesToWinAlice (gamesToWinBob - 1)
expectedWinforAliceSwitchServe Bob gamesToWinAlice gamesToWinBob
  | gamesToWinAlice < 0 || gamesToWinBob < 0 = error "Games to win should be non-negative"
  | otherwise =
      (1 / 3) * expectedWinforAliceSwitchServe Alice (gamesToWinAlice - 1) gamesToWinBob
        + (2 / 3) * expectedWinforAliceSwitchServe Alice gamesToWinAlice (gamesToWinBob - 1)

expectedWinforAliceWinnerServes :: Server -> Int -> Int -> Rational
expectedWinforAliceWinnerServes _ 0 _ = 1
expectedWinforAliceWinnerServes _ _ 0 = 0
expectedWinforAliceWinnerServes Alice gamesToWinAlice gamesToWinBob
  | gamesToWinAlice < 0 || gamesToWinBob < 0 = error "Games to win should be non-negative"
  | otherwise =
      0.75 * expectedWinforAliceWinnerServes Alice (gamesToWinAlice - 1) gamesToWinBob
        + 0.25 * expectedWinforAliceWinnerServes Bob gamesToWinAlice (gamesToWinBob - 1)
expectedWinforAliceWinnerServes Bob gamesToWinAlice gamesToWinBob
  | gamesToWinAlice < 0 || gamesToWinBob < 0 = error "Games to win should be non-negative"
  | otherwise =
      (1 / 3) * expectedWinforAliceWinnerServes Alice (gamesToWinAlice - 1) gamesToWinBob
        + (2 / 3) * expectedWinforAliceWinnerServes Bob gamesToWinAlice (gamesToWinBob - 1)

-- assign the result of the function to the main variable
main = do
  let switch = (expectedWinforAliceSwitchServe Alice 20 20)
  let winner = (expectedWinforAliceWinnerServes Alice 20 20)
  putStrLn $ show switch
  putStrLn $ show winner
  putStrLn $ display 1000 switch
