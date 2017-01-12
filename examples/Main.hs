module Main where

--------------------------------------------------------------------------------
import qualified Yoga as Yoga
--------------------------------------------------------------------------------

renderIntFn :: Yoga.LayoutInfo -> Int -> IO String
renderIntFn info x = do
  let tl = Yoga.nodePosition info
      pad = Yoga.nodePaddingLeft info
  putStrLn $ concat ["Node ", show x, ": ", show tl, " -- ", show pad]
  return ""

main :: IO ()
main = do
  let cs = [Yoga.exact 100.0 100.0 y | y <- [6..9]]
      cs2 = take 4 $ repeat $
            Yoga.paddingSetWith Yoga.Edge'Left 10.0
            (Yoga.exact 100.0 100.0) 23
  let tree = Yoga.vbox 3 $
             Yoga.startToEnd [
               Yoga.hbox 0 $ Yoga.startToEnd cs,
               Yoga.marginSetWith Yoga.Edge'Top 10.0 (Yoga.hbox 1) $
               Yoga.startToEnd cs,
               Yoga.hbox 2 $ Yoga.startToEnd cs2]
  _ <- Yoga.render tree renderIntFn
  return ()
