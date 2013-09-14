module Japex.Quiz.Common
    (
        QuizEntry(..)
        , splitCategories
    ) where

data QuizEntry = Quiz {
    japanese :: String
    , english :: String
    , categories :: [String]
}

splitCategories cs 
    | null rest = [cat]
    | otherwise = cat : splitCategories (tail rest)
    where (cat, rest) = break (== ',') cs

