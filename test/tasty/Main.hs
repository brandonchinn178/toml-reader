import Test.Tasty

import TOML.DecodeTest qualified
import TOML.ErrorTest qualified
import TOML.ParserTest qualified
import TOML.Utils.MapTest qualified
import TOML.Utils.NonEmptyTest qualified

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
