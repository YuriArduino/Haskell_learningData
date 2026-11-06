module Runner (runQuiz) where

import Parser (parseInt)
import Questions (allQuestions, Question (..), renderQuestion)

runQuiz :: IO ()
runQuiz = mapM_ runQuestion allQuestions

runQuestion :: Question -> IO ()
runQuestion q@(Question _ _ _ _ _ correct) = do
    let questionLines = renderQuestion q
    mapM_ putStrLn questionLines
    answer <- getAnswer

    if answer == correct
        then putStrLn "Correct!\n"
        else putStrLn
          ("Incorrect! The correct answer was "<> show correct <> "\n")
    where
        getAnswer = do
            answer <- parseInt <$> getLine
            case answer of
                Nothing -> putStrLn "Please enter a number 1-4!" >> getAnswer
                Just x -> return x
