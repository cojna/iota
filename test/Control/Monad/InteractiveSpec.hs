module Control.Monad.InteractiveSpec (main, spec) where

import           Control.Applicative
import           Control.Monad.Interactive hiding (Accepted, Failed, Running)
import           Data.Functor.Identity
import           Test.Prelude

type MockJudge a = Judge Identity a

data JudgeResult a
    = Running a
    | Accepted
    | Failed
    deriving (Eq, Show)

runMockJudge :: MockJudge () -> JudgeResult ()
runMockJudge m
    = runIdentity
    $ unJudge m (return Accepted) (return Failed) (return . Running)

accepted :: MockJudge a
accepted = Judge (\ac _ _ -> ac)

failed :: MockJudge a
failed = Judge (\_ wa _ -> wa)

running :: a -> MockJudge a
running x = Judge (\_ _ k -> k x)

running_ :: MockJudge ()
running_ = Judge (\_ _ k -> k ())

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Judge" $ do
        describe "Alternative" $ do
            it "AC <|> AC = AC" $ do
                runMockJudge (accepted <|> accepted)
                    `shouldBe` Accepted
            it "AC <|> WA = WA" $ do
                runMockJudge (accepted <|> failed)
                    `shouldBe` Failed
            it "AC <|> Running = Running" $ do
                runMockJudge (accepted <|> running_)
                    `shouldBe` Running ()
            it "WA <|> AC = WA" $ do
                runMockJudge (failed <|> accepted)
                    `shouldBe` Failed
            it "WA <|> WA = WA" $ do
                runMockJudge (failed <|> failed)
                    `shouldBe` Failed
            it "WA <|> Running = WA" $ do
                runMockJudge (failed <|> running_)
                    `shouldBe` Failed
            it "Running <|> AC = Running" $ do
                runMockJudge (running_ <|> accepted)
                    `shouldBe` Running ()
            it "Running <|> WA = Running" $ do
                runMockJudge (running_ <|> failed)
                    `shouldBe` Running ()
            it "Running <|> Running = Running" $ do
                runMockJudge (running_ <|> running_)
                    `shouldBe` Running ()
        describe "Monad" $ do
            it "AC >> AC = AC" $ do
                runMockJudge (accepted >> accepted)
                    `shouldBe` Accepted
            it "AC >> WA = AC" $ do
                runMockJudge (accepted >> failed)
                    `shouldBe` Accepted
            it "AC >> Running = AC" $ do
                runMockJudge (accepted >> running_)
                    `shouldBe` Accepted
            it "WA >> AC = WA" $ do
                runMockJudge (failed >> accepted)
                    `shouldBe` Failed
            it "WA >> WA = WA" $ do
                runMockJudge (failed >> failed)
                    `shouldBe` Failed
            it "WA >> Running = WA" $ do
                runMockJudge (failed >> running_)
                    `shouldBe` Failed
            it "Running >> AC = AC" $ do
                runMockJudge (running_ >> accepted)
                    `shouldBe` Accepted
            it "Running >> WA = WA" $ do
                runMockJudge (running_  >> failed)
                    `shouldBe` Failed
            it "Running >> Running = Running" $ do
                runMockJudge (running_ >> running_)
                    `shouldBe` Running ()
