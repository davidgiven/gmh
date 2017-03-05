{-# LANGUAGE ScopedTypeVariables #-}
module Flags where
import qualified Data.StringMap as StringMap

type SetsValueWith r t = r -> t -> r
type SetsValue r = SetsValueWith r String
type SetsRest r = SetsValueWith r [String]

data Flag r =
    Flag {
        names :: [String],
        setter :: SetsValue r,
        consumes :: Bool
    }

stringFlag :: [String] -> SetsValueWith r String -> Flag r
stringFlag names setter = Flag names setter True

boolFlag :: [String] -> SetsValueWith r Bool -> Flag r
boolFlag names setter = Flag names wrappedSetter False
    where
        wrappedSetter old value = setter old True

parse :: forall r. r -> [String] -> [Flag r] -> r
parse values argv flagDescription =
    accumulate values argv
    where
        accumulate :: r -> [String] -> r
        accumulate values [] = values
        accumulate values (first:[]) = doAccumulate values first "" []
        accumulate values (first:second:argv) = doAccumulate values first second argv

        doAccumulate :: r -> String -> String -> [String] -> r
        doAccumulate values first@('-':_) second argv =
            -- this is a flag; parse it.
            case (StringMap.lookup first flagMap) of
                Just flag -> recurse newValues (consumes flag)
                    where
                        newValues = (setter flag) values second
                Nothing -> error ("flag '" ++ first ++ "' is not recognised")
            where
                recurse newValues False = accumulate newValues (second:argv)
                recurse newValues True = accumulate newValues argv

        doAccumulate values _ second argv =
            -- this is not a flag; stop parsing here.
            values

        flagMap =
            foldl insertFlags StringMap.empty flagDescription
        insertFlags map arg =
            foldl insertFlag map (names arg)
                where
                    insertFlag map name =
                        StringMap.insert name arg map
