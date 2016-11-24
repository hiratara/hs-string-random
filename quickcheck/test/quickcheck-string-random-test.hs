{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Test.QuickCheck as QC
import Test.QuickCheck.StringRandom (matchRegexp)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as TastyQC

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "by quickcheck"
  [ TastyQC.testProperty "generate digit" prop_generateDigit
  , TastyQC.testProperty "generate upper case" prop_generateUpper
  ]

prop_generateDigit :: QC.Property
prop_generateDigit = QC.forAll (matchRegexp "\\d") $ \digit ->
  Text.length digit == 1 && Char.isDigit (Text.head digit)

newtype Upper = Upper Text.Text deriving (Eq, Show)

instance QC.Arbitrary Upper where
  arbitrary = Upper <$> matchRegexp "[A-Z]"

prop_generateUpper :: Upper -> Bool
prop_generateUpper (Upper up) = Text.length up == 1 && Char.isUpper (Text.head up)
