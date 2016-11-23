{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Control.Monad (forM_)
import qualified Text.StringRandom as StringRandom
import qualified Text.StringRandom.Parser as Parser
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import Test.Tasty.HUnit ((@?=), (@?))
import qualified Text.Regex.PCRE.Heavy as PCRE

main :: IO ()
main = Tasty.defaultMain allTests

allTests :: Tasty.TestTree
allTests = Tasty.testGroup "All tests of random-string"
  [ testParser
  , testStringRandom
  ]

testParser :: Tasty.TestTree
testParser = HUnit.testCase "Test parsers" $ do
  Parser.processParse "" @?= Right (Parser.PConcat [])
  Parser.processParse "a" @?= Right (Parser.PClass "a")
  Parser.processParse "[a-c]" @?= Right (Parser.PClass "abc")
  Parser.processParse "\\d" @?= Right (Parser.PClass "0123456789")
  Parser.processParse "[^\\w\\W]" @?= Right (Parser.PClass "")

  Parser.processParse "a*" @?= Right (Parser.PRange
                                       0 Nothing (Parser.PClass "a"))
  Parser.processParse "a+" @?= Right (Parser.PRange
                                       1 Nothing (Parser.PClass "a"))
  Parser.processParse "a{3}" @?= Right (Parser.PRange
                                         3 (Just 3) (Parser.PClass "a"))
  Parser.processParse "a{1,}" @?= Right (Parser.PRange
                                          1 Nothing (Parser.PClass "a"))
  Parser.processParse "a{1,3}" @?= Right (Parser.PRange
                                           1 (Just 3) (Parser.PClass "a"))

  Parser.processParse "abc" @?= Right (Parser.PConcat
                                       [ Parser.PClass "a"
                                       , Parser.PClass "b"
                                       , Parser.PClass "c"
                                       ])

  Parser.processParse "a|b|c" @?= Right (Parser.PSelect
                                         [ Parser.PClass "a"
                                         , Parser.PClass "b"
                                         , Parser.PClass "c"
                                         ])

  Parser.processParse "((a)(?:b)(c))" @?=
    Right (Parser.PGrouped 1
              ( Parser.PConcat
                [ Parser.PGrouped 2 (Parser.PClass "a")
                , Parser.PClass "b"
                , Parser.PGrouped 3 (Parser.PClass "c")
                ]
              )
          )

  Parser.processParse "\\5" @?= Right (Parser.PBackward 5)

  Parser.processParse "^a\\b$" @?= Right (Parser.PConcat
                                          [ Parser.PIgnored
                                          , Parser.PClass "a"
                                          , Parser.PIgnored
                                          , Parser.PIgnored
                                          ])

testStringRandom :: Tasty.TestTree
testStringRandom = HUnit.testCase "Test stringRandomIO" $ do
  e <- StringRandom.stringRandomIO ""
  e @?= ""

  e' <- StringRandom.stringRandomIO "a{0}"
  e' @?= ""

  let patterns =
        [ ""
        , "a"
        , "[a-c]"
        , "\\d"
        , "[^\\W]"
        , "a*"
        , "a+"
        , "a{4}"
        , "a{2,}"
        , "a{2,4}"
        , "abc"
        , "a|b|c"
        , "((a)(?:b)(c))"
        , "(.*)\\1"
        ]
  forM_ patterns $ \pat -> do
    let pat' = "^" <> fixBS pat <> "$"
        (Right reg) = PCRE.compileM pat' []
    randbs <- fixBS <$> StringRandom.stringRandomIO pat
    randbs PCRE.=~ reg @? ("check regexp: " ++ show pat'
                            ++ ", string: " ++ show randbs)

      where
        -- Seems that PCRE can't handle strings generated by encodeUtf8
        fixBS = (<> "") . Encoding.encodeUtf8
