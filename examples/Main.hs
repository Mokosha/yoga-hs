module Main where

--------------------------------------------------------------------------------
import Yoga as Yoga
--------------------------------------------------------------------------------

main :: IO ()
main = do
  node <- newNode
  resetNode node
  putStrLn "I did it!"
