-- vim: syntax=hspec

import           Data.Foldable  (traverse_)
import           ForeignHash    (foreignFileHash)
import           PerceptualHash (fileHash)
import           Test.Hspec

checkForeign :: FilePath -> SpecWith ()
checkForeign fp =
    parallel $ it ("should return the same hash (" ++ fp ++ ")") $ do
        actual <- fileHash fp
        expected <- foreignFileHash fp
        actual `shouldBe` expected

main :: IO ()
main = hspec $
    describe "fileHash" $ do

        parallel $ it "should match when same" $ do
            actual <- fileHash "demo-data/frog.jpeg"
            expected <- fileHash "demo-data/frog.png"
            actual `shouldBe` expected

        parallel $ it "should not match when different" $ do
            actual <- fileHash "demo-data/cat.png"
            expected <- fileHash "demo-data/frog.png"
            actual `shouldSatisfy` (/= expected)

        traverse_ checkForeign
            [ "demo-data/frog.jpeg"
            , "demo-data/frog.png"
            , "demo-data/cat.png"
            ]
