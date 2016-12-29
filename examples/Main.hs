module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

renderIntFn :: (Float, Float) -> (Float, Float) -> Int -> IO String
renderIntFn (left, top) _ x = do
  putStrLn $ concat ["Node ", show x, ": (", show left, ", ", show top, ")"]
  return ""

main :: IO ()
main = do
  let cs = [Yoga.exact x 100.0 y | x <- [100.0,200.0..400.0], y <- [1..]]
  let node = Yoga.hbox (Yoga.startToEnd cs) 0
  _ <- Yoga.render node renderIntFn
  return ()
