module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

renderIntFn :: Yoga.LayoutInfo -> Int -> IO String
renderIntFn info x = do
  let (top, left) = Yoga.topLeft info
  putStrLn $ concat ["Node ", show x, ": (", show left, ", ", show top, ")"]
  return ""

main :: IO ()
main = do
  let cs = [Yoga.exact 100.0 100.0 y | y <- [6..9]]
      cs2 = take 4 $ repeat $ Yoga.exact 100.0 100.0 23
  let tree = Yoga.vbox (Yoga.startToEnd [
                           Yoga.hbox (Yoga.startToEnd cs) 0,
                           Yoga.hbox (Yoga.startToEnd cs) 1,
                           Yoga.hbox (Yoga.startToEnd cs2) 2]) 3
  _ <- Yoga.render tree renderIntFn
  return ()
