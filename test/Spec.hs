import Test.Hspec

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)

import Control.Applicative (pure, some)

import Control.Monad.Identity (Identity)
import Control.Monad
import Data.List
import Data.Char (isAlpha)






realExample :: [Char]
realExample = intercalate "\n"
      [ "{-# LANGUAGE TemplateHaskell #-}"
      , "{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}"
      , ""
      , "module Freckle.Entities.Ela.Decodables.AssignmentSession"
      , "  ( module Freckle.Entities.Ela.Decodables.AssignmentSession"
      , "  ) where"
      , ""
      , "import Freckle.Entities.Import"
      , ""
      , "import Database.Persist.Sql (toSqlKey)"
      , "import Test.QuickCheck.Arbitrary.Generic"
      , ""
      , "mkPersist"
      , "  sqlSettings"
      , "  [persistLowerCase|"
      , "ElaDecodablesAssignmentSession sql=ela_decodables_assignment_sessions"
      , "  studentId  StudentId"
      , "  assignmentId  ElaDecodablesAssignmentId"
      , "  updatedAt UTCTime"
      , "  completedAt  UTCTime Maybe sql=completed_at"
      , "  durationSeconds  DurationSeconds Maybe"
      , "  accuracy  (Percentage Double) Maybe"
      , "  numQuestionsAnswered  OverflowNatural Maybe"
      , "  ElaDecodablesAssignmentSessionsStudentIdAssignmentIdKey studentId assignmentId"
      , "  deriving Show Eq Ord Generic"
      , "|]"
      , ""
      , ""
      ]

realExample2 :: [Char]
realExample2 = intercalate "\n"
      [ "{-# LANGUAGE TemplateHaskell #-}"
      , "{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}"
      , ""
      , "module Freckle.Entities.Ela.Decodables.AssignmentSession"
      , "  ( module Freckle.Entities.Ela.Decodables.AssignmentSession"
      , "  ) where"
      , ""
      , "import Freckle.Entities.Import"
      , ""
      , "import Database.Persist.Sql (toSqlKey)"
      , "import Test.QuickCheck.Arbitrary.Generic"
      , ""
      , "mkPersist"
      , "  sqlSettings"
      , "  [persistLowerCase| ElaDecodablesAssignmentSession"
      , "sql=ela_decodables_assignment_sessions"
      , "  studentId  StudentId"
      , "  assignmentId  ElaDecodablesAssignmentId"
      , "  updatedAt UTCTime"
      , "  completedAt  UTCTime Maybe sql=completed_at"
      , "  durationSeconds  DurationSeconds Maybe"
      , "  accuracy  (Percentage Double) Maybe"
      , "  numQuestionsAnswered  OverflowNatural Maybe"
      , "  ElaDecodablesAssignmentSessionsStudentIdAssignmentIdKey studentId assignmentId"
      , "  deriving Show Eq Ord Generic"
      , "|]"
      , ""
      , ""
      ]




data CtagPieces = CtagPieces [(String, Integer)]

parse :: Parsec String () a -> String -> Either ParseError a
parse rule text = Parsec.parse rule "(source)" text

matchTillNewline = manyTill anyChar (try (string "\n"))
matchTillEOFNoNewline = manyTill (noneOf "\n") (try eof)

matchAnyCharTillNewline' :: Parsec String Integer ()
matchAnyCharTillNewline' = do
  manyTill anyChar (try (string "\n"))
  modifyState (+1)

matchSpaceTillNewline' :: Parsec String Integer ()
matchSpaceTillNewline' = do
  manyTill space' (try (string "\n"))
  modifyState (+1)

parseBeginning :: Parsec String Integer ()
parseBeginning = do
  let matchLineWithPersistLowerCase = try (manyTill (noneOf "\n") (try (string "[persistLowerCase|")))
  void $ manyTill matchAnyCharTillNewline' matchLineWithPersistLowerCase

space' :: Parsec String a Char
space' = (try (char ' ') <|> try (char '\t'))

alpha :: Parsec String a Char
alpha = satisfy isAlpha

entityPiece :: Parsec String a String
entityPiece = do
  u <- upper
  l <- many alpha
  pure $ u : l

parseEntityName :: Parsec String a String
parseEntityName = do
  concat <$> some entityPiece

parseEntity :: Parsec String Integer CtagPieces
parseEntity = do
  many matchSpaceTillNewline'
  entityName <- manyTill space' (try parseEntityName)
  entityNameLineNum <- getState


  pure $ CtagPieces [(entityName, entityNameLineNum)]

  -- matchTillNewline'
  -- undefined
  -- i <- getState
  -- pure i

-- parseEntityLineNumbers :: String -> Either ParseError [(String, Integer)]
-- parseEntityLineNumbers input =
--   let x =
--   runParser x (lineParser 1) 1 "filename.hs" input

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

  it "parses possibly" $ do
    runParser (parseBeginning >> many anyChar >> eof >> getState) 1 "(source)" realExample `shouldBe` Right 15
