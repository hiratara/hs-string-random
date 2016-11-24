{-|
Module      : Text.QuickCheck.StringRandom
Description : A generator of text which matches regexp
Copyright   : Copyright (C) 2016- hiratara
License     : GPL-3
Maintainer  : hiratara@cpan.org
Stability   : experimental

A text generator that generates a string that matches a regular expression.

@
    {-# LANGUAGE OverloadedStrings #-}
    import qualified Test.QuickCheck as QC
    import Test.QuickCheck.StringRandom (matchRegexp)

    prop_generateDigit :: QC.Property
    prop_generateDigit = QC.forAll (matchRegexp "\\d") $ \digit -> ...

    -- or

    newtype Upper = Upper Text.Text deriving (Eq, Show)

    instance QC.Arbitrary Upper where
      arbitrary = Upper <$> matchRegexp "[A-Z]"

    prop_generateUpper :: Upper -> Bool
    prop_generateUpper (Upper upper) = ...
@

The shrink function has not been defined yet.
-}

module Test.QuickCheck.StringRandom
  ( matchRegexp
  ) where

import qualified Data.Text as Text
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import qualified Text.StringRandom as StringRandom

-- | The 'matchRegexp pat' defines a generator that produces a text that
-- | matches a regular expression 'pat'
matchRegexp :: Text.Text -> QC.Gen Text.Text
matchRegexp txt = QC.MkGen $ \(QC.QCGen g) _ -> StringRandom.stringRandom g txt
