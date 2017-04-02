module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

block :: Int -> Yoga.Layout Int
block = Yoga.exact 100.0 100.0

renderIntFn :: Yoga.LayoutInfo -> Int -> IO String
renderIntFn info x = do
  let tl = Yoga.nodePosition info
      pad = Yoga.nodePaddingLeft info
  putStrLn $ concat ["Node ", show x, ": ", show tl, " -- ", show pad]
  return ""

main :: IO ()
main = do
  let cs = [ block y | y <- [6..9]]
      cs2 = take 4 $ repeat $ Yoga.withPadding Yoga.Edge'Left 10.0 block 23
  let tree =
        Yoga.vbox (
          Yoga.startToEnd [
             Yoga.hbox (Yoga.startToEnd cs) 0,
             Yoga.withMargin Yoga.Edge'Top 10.0 (Yoga.hbox (Yoga.startToEnd cs)) 1,
             Yoga.hbox (Yoga.startToEnd cs2) 2]
          ) 3
  _ <- Yoga.render tree renderIntFn
  return ()
