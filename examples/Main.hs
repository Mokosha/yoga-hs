module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

renderIntFn :: (Float, Float) -> (Float, Float) -> Int -> IO String
renderIntFn _ _ x = do
  print x
  return $ concat ["Node ", show x]

renderStringFn :: (Float, Float) -> (Float, Float) -> String -> IO ()
renderStringFn _ _ str = putStrLn str

main :: IO ()
main = do
  let node = Yoga.node 100.0 100.0 0
  a' <- Yoga.render node renderIntFn
  _ <- Yoga.render a' renderStringFn
  return ()
