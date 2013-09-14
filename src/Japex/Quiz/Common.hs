module Japex.Quiz.Common
    (
        QuizEntry(..)
        , AnswerEntry(..)
        , splitCategories
    ) where

import qualified Data.Text as T

data QuizEntry = Quiz {
    japanese :: T.Text
    , english :: T.Text
    , categories :: [T.Text]
}

data AnswerEntry = Answer {
    question :: T.Text
    , userAnswer :: T.Text
    , correctAnswer :: T.Text
}

splitCategories = T.split (==',')
