module Main where

--------------------------------------------------------------------------------
import Yoga as Yoga
--------------------------------------------------------------------------------

renderIntFn :: (Float, Float) -> (Float, Float) -> Int -> IO String
renderIntFn _ _ x = do
  print x
  return $ concat ["Node ", show x]

renderStringFn :: (Float, Float) -> (Float, Float) -> String -> IO ()
renderStringFn _ _ str = putStrLn str

main :: IO ()
main = do
  let node = mkNode 0
  a' <- render node renderIntFn
  _ <- render a' renderStringFn
  return ()
