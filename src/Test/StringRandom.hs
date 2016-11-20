{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
-- https://github.com/cho45/String_random.js/blob/master/lib/String_random.js
module Test.StringRandom
  ( stringRandom
  ) where

import qualified Test.StringRandom.Parser as Parser
import qualified System.Random as Random
import qualified Data.Text as Text

stringRandom :: Random.RandomGen r => r -> Text.Text -> Text.Text
stringRandom = undefined
