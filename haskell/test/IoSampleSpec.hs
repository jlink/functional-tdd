import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import IoSample

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Lib.getNameAndGreet" $ do
    it "input should be used for greeting formula" $ do
      let fake = ioFake "fake name"
      let mock = ioMock "Welcome to Haskell, fake name!"
      getNameAndGreet fake mock


ioFake :: a -> IO a
ioFake name = do
  return name

ioMock :: String -> (String -> IO ())
ioMock expected actual = do
  if expected == actual
  then
    putStrLn $ "IO Success with: " ++ show(expected)
  else
    ioError (userError $ "Expected: " ++ show(expected) ++ " but was: " ++ show(actual))
