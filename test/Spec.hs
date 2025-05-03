import CompilationTest as CT


import Test.Hspec


main :: IO ()
main = hspec $ do
  CT.machineRodinASTTest
  CT.contextRodinASTTest
