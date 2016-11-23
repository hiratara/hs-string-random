{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Test.QuickCheck as QC
import Test.QuickCheck.StringRandom (matchRegexp)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as TastyQC

main :: IO ()
main = Tasty.defaultMain (TastyQC.testProperty
                           "by quickcheck" prop_generateDigit)

prop_generateDigit :: QC.Property
prop_generateDigit = QC.forAll (matchRegexp "\\d") $ \digit ->
  Text.length digit == 1 && Char.isDigit (Text.head digit)
