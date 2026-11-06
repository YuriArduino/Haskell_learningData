module Questions  (Question(..), allQuestions, renderQuestion) where

data Question = Question
    {
    prompt          :: String
    , answer1       :: String
    , answer2       :: String
    , answer3       :: String
    , answer4       :: String
    , correctAnswer :: Int
    }

question1 :: Question
question1 = Question
    "What is the type signature for 'sortOn' ?"
    "(a -> a -> Bool) -> [a] -> [a]"
    "Ord a => [a] -> [a]"
    "(a -> a -> Ordering) -> [a] -> [a]"
    "Ord b => (a -> b) -> [a] -> [a]"
    4

question2 :: Question
question2 = Question
    "What is the result of the expression 'delete 3 [2, 3, 0, 3, 2, 3]'"
    "[2, 0, 3, 2, 3]"
    "[2, 0, 2]"
    "[2, 3, 0, 3, 2, 3]"
    "[2, 3, 0, 2, 3]"
    1

allQuestions :: [Question]
allQuestions = [question1, question2]

renderQuestion :: Question -> [String]
renderQuestion (Question p a b c d _) =
    [ p
    , "1: " <> a
    , "2: " <> b
    , "3: " <> c
    , "4: " <> d
    ]
