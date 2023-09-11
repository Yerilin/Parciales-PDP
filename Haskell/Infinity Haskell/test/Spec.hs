import Test.Hspec
import Parcial

main :: IO ()
main = hspec $ do
  describe "Sumar" $ do
    it "sumar 2 y 2 da 5" $ do
      (sumar 2 2) `shouldBe` 4