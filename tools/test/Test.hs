import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Util.Tripping (runTests)
import qualified Test.Pos.Tools.Dbgen.Json

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Tools.Dbgen.Json.tests
        ]
