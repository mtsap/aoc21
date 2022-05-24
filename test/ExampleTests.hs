module ExampleTests (
  nodeTest1,
) where

import Test.HUnit

addTest = TestCase (assertEqual "mkNode that fails" (2) (add 1 1))
