module Japex.Quiz.File
    (
        readQuizDatabaseFile
    ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Time.Clock
import Data.Time.Format
import Japex.Common
import Japex.Quiz.Common (splitCategories)
import System.FilePath
import System.IO.Error
import System.Locale
import Text.Parsec.Extra
import Text.Parsec.Prim (Parsec)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import qualified Data.Text as T

readQuizDatabaseFile :: FilePath -> IO [QuizEntry]
readQuizDatabaseFile file = do
    input <- readFile file
    either (fail . show) return (runParser quizDatabaseParser () file input)

quizDatabaseParser :: GenParser Char () [QuizEntry]
quizDatabaseParser = endBy line eol
line = do
    jap <- T.pack <$> some (noneOf ":") <* char ':'
    eng <- T.pack <$> some (noneOf ":") <* char ':'
    cats <- map T.pack <$> some (noneOf ":,\n") `sepBy1` char ','
    return $ Quiz jap eng cats
