import Test.Tasty
import qualified FlagsTests as FlagsTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ FlagsTests.tests ]
