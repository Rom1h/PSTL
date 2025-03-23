import DataExprSpec as DES


import Test.Hspec


main :: IO ()
main = hspec $ do
  -- revrev
  DES.dataExprTest
