

import Test.Hspec

import qualified Text.Parsec  as Parsec
import Text.Parsec hiding (parse)

import Control.Applicative

import Control.Monad.Identity (Identity)

parse :: Parsec String () a -> String -> Either ParseError a
parse rule text = Parsec.parse rule "(source)" text

main :: IO ()
main = hspec $ do
  it "whatever" $ do
    'a' `shouldBe` 'b'
