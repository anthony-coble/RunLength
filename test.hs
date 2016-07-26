import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import RunLength

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
  [ TestList $ encodeTests ++ decodeTests
  ]

encodeTests :: [Test]
encodeTests = [ testCase "empty string returns empty" $ "" @=? encode ""
              , testCase "simple string gets encoded" $ "3A" @=? encode "AAA"
              , testCase "more complicated string" $ "1H1O1R1S1E" @=? encode "HORSE"
              , testCase "an even more complex string" $ "12W1B12W3B24W1B" @=? encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
              ]

decodeTests :: [Test]
decodeTests = [ testCase "it decodes an encoded simple string" $ "AAA" @=? decode "3A"
              , testCase "it decodes a more complicated string" $ "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB" @=? decode "12W1B12W3B24W1B"
              ]

