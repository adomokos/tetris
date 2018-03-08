module Tetris.LogicSpec where

import Test.Hspec
import qualified Tetris.Logic as L

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $ do
        it "can position elements" $ do
            L.someFunc `shouldBe` "someFunc"
