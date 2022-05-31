import Test.Tasty

import qualified TOML.DecodeTest
import qualified TOML.Utils.MapTest
import qualified TOML.Utils.NonEmptyTest

main :: IO ()
main =
  defaultMain $
    testGroup "toml-reader" $
      -- TODO: test megaparsec error messages
      -- TODO: test normalize error messages
      [ TOML.Utils.MapTest.test
      , TOML.Utils.NonEmptyTest.test
      , TOML.DecodeTest.test
      ]
