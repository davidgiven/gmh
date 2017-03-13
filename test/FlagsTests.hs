module FlagsTests where
import Test.Tasty
import Test.Tasty.HUnit
import qualified Flags as Flags
import Data.Text (Text)
import qualified Data.Text as Text

data Options =
    Options {
        boolOption :: Bool,
        intOption :: Int,
        textOption :: Text
    } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions =
    Options {
        boolOption = False,
        intOption = 0,
        textOption = ""
    }

setBoolOption options value = options { boolOption = value }
setIntOption options value = options { intOption = value }
setTextOption options value = options { textOption = value }

boolTests = testCase "boolTests" $ do
    assertEqual "flagByItself"
        (Flags.ParsedFlags (defaultOptions { boolOption = True }) [])
        (Flags.parse defaultOptions ["--bool-option"] description)
    assertEqual "flagWithTrailingArg"
        (Flags.ParsedFlags (defaultOptions { boolOption = True }) ["fnord"])
        (Flags.parse defaultOptions ["--bool-option", "fnord"] description)
    where
        description :: [Flags.Flag Options]
        description =
            [
                Flags.boolFlag ["--bool-option"] setBoolOption
            ]

anyTests = testCase "anyTests" $ do
    assertEqual "splitFlag"
        (Flags.ParsedFlags (defaultOptions { intOption = 1 }) [])
        (Flags.parse defaultOptions ["--int-option", "1"] description)
    assertEqual "combinedFlag"
        (Flags.ParsedFlags (defaultOptions { intOption = 1 }) [])
        (Flags.parse defaultOptions ["--int-option=1"] description)
    assertEqual "splitFlagWithTrailing"
        (Flags.ParsedFlags (defaultOptions { intOption = 1 }) ["fnord"])
        (Flags.parse defaultOptions ["--int-option", "1", "fnord"] description)
    assertEqual "combinedFlagWithTrailing"
        (Flags.ParsedFlags (defaultOptions { intOption = 1 }) ["fnord"])
        (Flags.parse defaultOptions ["--int-option=1", "fnord"] description)
    where
        description :: [Flags.Flag Options]
        description =
            [
                Flags.anyFlag ["--int-option"] setIntOption
            ]

textTests = testCase "textTests" $ do
    assertEqual "splitFlag"
        (Flags.ParsedFlags (defaultOptions { textOption = "q" }) [])
        (Flags.parse defaultOptions ["--text-option", "q"] description)
    assertEqual "combinedFlag"
        (Flags.ParsedFlags (defaultOptions { textOption = "q" }) [])
        (Flags.parse defaultOptions ["--text-option=q"] description)
    assertEqual "splitFlagWithTrailing"
        (Flags.ParsedFlags (defaultOptions { textOption = "q" }) ["fnord"])
        (Flags.parse defaultOptions ["--text-option", "q", "fnord"] description)
    assertEqual "combinedFlagWithTrailing"
        (Flags.ParsedFlags (defaultOptions { textOption = "q" }) ["fnord"])
        (Flags.parse defaultOptions ["--text-option=q", "fnord"] description)
    where
        description :: [Flags.Flag Options]
        description =
            [
                Flags.textFlag ["--text-option"] setTextOption
            ]

tests = testGroup "Flags"
    [
        boolTests,
        anyTests,
        textTests
    ]
