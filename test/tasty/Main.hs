import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup "toml-reader" $
      -- TODO: test decode logic
      -- TODO: test parse error messages
      []
