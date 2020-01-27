-- vim: syntax=hspec

import           PerceptualHash (fileHash)
import           Test.Hspec

main :: IO ()
main = hspec $
    describe "fileHash" $ do

        parallel $ it "should match when same" $ do
            actual <- fileHash "demo-data/frog.jpeg"
            expected <- fileHash "demo-data/frog.png"
            actual `shouldBe` expected

        parallel $ it "should match when same" $ do
            actual <- fileHash "demo-data/meme-watermark.jpg"
            expected <- fileHash "demo-data/meme.png"
            actual `shouldBe` expected

        parallel $ it "should not match when different" $ do
            actual <- fileHash "demo-data/cat.png"
            expected <- fileHash "demo-data/frog.png"
            actual `shouldSatisfy` (/= expected)
