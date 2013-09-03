module Japex.Review
    (reviewCommand)
where

import System.IO
import Japex.Common

reviewCommand = Command doReview (putStrLn "review")

doReview args = do
    resFile <- parseArgs args
    results <- readFile resFile
    let reviewedResults = review . parseResults $ results
    writeFile "review.txt" $ format reviewedResults
    putStrLn . summarize $ reviewedResults

parseArgs [] = ioError $ userError "Review file not supplied."
parseArgs as = return $ head as

parseResults ls = map extractFields $ lines ls

extractFields l = (q, a, ca)
    where (q, as) = breakField l
          (a, ca') = breakField $ tail as
          ca = tail ca'
          breakField = break (== ':')

review = map reviewSingle
    where reviewSingle (q, a, ca) = (q, a, ca, a == ca)

format = unlines . map resultToString
    where resultToString (q, a, ca, c)
            | c = q ++ " correct, is " ++ a
            | otherwise = q ++ " incorrect, guessed " ++ a ++ ", is " ++ ca
          
summarize rs = show a ++ "/" ++ show b ++ " correct answers"
    where (a, b) = foldr accumCorrect (0, 0) rs
          accumCorrect (_,_,_,c) (ct,t)
            | c = (ct + 1, t + 1)
            | otherwise = (ct, t + 1)
