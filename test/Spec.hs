

import Test.Hspec

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)

import Control.Applicative (pure)

import Control.Monad.Identity (Identity)
import Control.Monad
import Data.List

parse :: Parsec String () a -> String -> Either ParseError a
parse rule text = Parsec.parse rule "(source)" text

matchTillNewline = manyTill anyChar (try (string "\n"))
matchTillEOFNoNewline = manyTill (noneOf "\n") (try eof)

parseLinesState :: String -> Either ParseError (Integer, Integer)
parseLinesState input = do
  let
    matchLine i = do
      void $ matchTillNewline
      modifyState (+1)
      lineParser (i + 1)
    lineParser :: Integer -> Parsec String Integer (Integer, Integer)
    lineParser i =
      try (matchTillEOFNoNewline >> handleEnd i) <|> matchLine i
    handleEnd i = do
      j <- getState
      pure (i,j)
  runParser (lineParser 1) 1 "filename.hs" input

parseLines :: String -> Either ParseError Integer
parseLines input = do
  let
    lineParser i =
      try (matchTillEOFNoNewline >> pure i) <|>
         (matchTillNewline >> lineParser (i+1))
  parse (lineParser 1) input

parseStatic :: String -> Either ParseError Integer
parseStatic input = do
  let
    rules = do
      void $ matchTillNewline
      -- void $ manyTill anyChar eof
      pure 10
  parse rules input

main :: IO ()
main = hspec $ do
  it "whatever" $ do
    let input = intercalate "\n"
                  [ "a"
                  , "b"
                  , "c"
                  ]
    parseStatic input `shouldBe` Right 10
    parseLines input `shouldBe` Right 3
    let input2 = intercalate "\n"
                  [ "a"
                  , "b"
                  , "c"
                  , "d"
                  ]
    parseLinesState input2 `shouldBe` Right (4,4)
