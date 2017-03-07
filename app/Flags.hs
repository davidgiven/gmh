module Flags where
import qualified Data.Map.Strict as Map
import Data.Text as Text
import Data.Monoid

type SetsValueWith r t = r -> t -> r
type SetsValue r = SetsValueWith r Text
type SetsRest r = SetsValueWith r [Text]

data Flag r =
    Flag {
        names :: [Text],
        setter :: SetsValue r,
        consumes :: Bool
    }

data ParsedFlags r =
    ParsedFlags {
        values :: r,
        rest :: [Text]
    }

textFlag :: [Text] -> SetsValueWith r Text -> Flag r
textFlag names setter = Flag names setter True

boolFlag :: [Text] -> SetsValueWith r Bool -> Flag r
boolFlag names setter = Flag names wrappedSetter False
    where
        wrappedSetter old value = setter old True

parse :: forall r. r -> [Text] -> [Flag r] -> ParsedFlags r
parse values argv flagDescription =
    accumulate (ParsedFlags values argv)
    where
        accumulate :: ParsedFlags r -> ParsedFlags r
        accumulate p@(ParsedFlags values []) = p
        accumulate (ParsedFlags values (first:[])) = doAccumulate values first "" []
        accumulate (ParsedFlags values (first:second:argv)) = doAccumulate values first second argv

        doAccumulate :: r -> Text -> Text -> [Text] -> ParsedFlags r
        doAccumulate values first second argv
            | (Text.head first) == '-' =
                case (Map.lookup first flagMap) of
                    Just flag -> recurse newValues (consumes flag)
                        where
                            newValues = (setter flag) values second
                            recurse newValues False = accumulate (ParsedFlags newValues (second:argv))
                            recurse newValues True = accumulate (ParsedFlags newValues argv)
                    Nothing -> error $ unpack ("flag '" <> first <> "' is not recognised")
            | (second == "") && (argv == []) =
                ParsedFlags values [first]
            | otherwise =
                ParsedFlags values (first:second:argv)

        flagMap :: Map.Map Text (Flag r)
        flagMap =
            Prelude.foldl insertFlags Map.empty flagDescription
        insertFlags map arg =
            Prelude.foldl insertFlag map (names arg)
                where
                    insertFlag map name =
                        Map.insert name arg map
