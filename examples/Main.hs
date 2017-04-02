module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

block :: Int -> Yoga.Layout Int
block = Yoga.exact 100.0 100.0

renderIntFn :: Yoga.LayoutInfo -> Int -> IO String
renderIntFn info x = do
  putStr $ concat ["Node ", show x, ": "]
  print info
  return ""

main :: IO ()
main = do
  let cs = [ block y | y <- [6..9]]
      cs2 = take 4 $ repeat $ Yoga.withPadding Yoga.Edge'Left 10.0 block 23
      mkHbox = Yoga.hbox . Yoga.startToEnd
  let tree =
        flip Yoga.vbox 3 $
        Yoga.startToEnd [
          mkHbox cs 0,
          ($ 1) (Yoga.withMargin Yoga.Edge'Top 10.0 $ mkHbox cs),
          mkHbox cs2 2]
  _ <- Yoga.render tree renderIntFn
  return ()
