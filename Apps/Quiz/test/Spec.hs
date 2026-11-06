import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Parser (parseInt)


main :: IO ()
main = defaultMain parseTests


parseTests :: TestTree
parseTests = testGroup "Parse Tests"
    [
         testCase "Parse1" $ parseInt "1"     @?= Just 1
        ,testCase "Parse2" $ parseInt "  2  " @?= Just 2
        ,testCase "Parse3" $ parseInt "34"    @?= Nothing
        ,testCase "Parse4" $ parseInt "-1"    @?= Nothing
        ,testCase "Parse5" $ parseInt " A"    @?= Nothing
    ]
