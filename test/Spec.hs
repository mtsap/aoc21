import ExampleTests
import Test.HUnit

exampleTests = TestList [addTest]

main :: IO ()
main = do
  runTestTT exampleTests
  putStrLn "Test finished"
