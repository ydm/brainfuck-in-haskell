import Lib
import Test.HUnit


test1 = TestCase (assertEqual "for extractLoop," "[qwe]" $
                   extractLoop "asd[qwe]asd")

tests = TestList [TestLabel "parsing" test1]

main :: IO ()
main = runTestTT tests
