

import Test.Hspec

import qualified Text.Parsec  as Parsec
import Text.Parsec hiding (parse)

import Control.Applicative

import Control.Monad.Identity (Identity)

import Data.List

parse :: Parsec String () a -> String -> Either ParseError a
parse rule text = Parsec.parse rule "(source)" text

parseStatic :: String -> Either ParseError Integer
parseStatic input = do
  let
    rules = do
      manyTill anyChar (try (string "\n"))
      manyTill anyChar (try (string "\n"))
      manyTill anyChar eof
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
