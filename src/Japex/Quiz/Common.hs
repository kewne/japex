module Japex.Quiz.Common
    (
        splitCategories
    ) where

import qualified Data.Text as T

splitCategories = T.split (==',')
