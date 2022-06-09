import Test.Tasty

import qualified TOML.DecodeTest
import qualified TOML.ErrorTest
import qualified TOML.ParserTest
import qualified TOML.Utils.MapTest
import qualified TOML.Utils.NonEmptyTest

main :: IO ()
main =
  defaultMain $
    testGroup "toml-reader" $
      [ TOML.Utils.MapTest.test
      , TOML.Utils.NonEmptyTest.test
      , TOML.ErrorTest.test
      , TOML.ParserTest.test
      , TOML.DecodeTest.test
      ]
