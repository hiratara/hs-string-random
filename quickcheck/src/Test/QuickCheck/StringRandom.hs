module Test.QuickCheck.StringRandom
  ( matchRegexp
  ) where

import qualified Data.Text as Text
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import qualified Text.StringRandom as StringRandom

matchRegexp :: Text.Text -> QC.Gen Text.Text
matchRegexp txt = QC.MkGen $ \(QC.QCGen g) _ -> StringRandom.stringRandom g txt
