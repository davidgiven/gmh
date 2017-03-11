module UI where
import Data.Text

init :: IO ()
init = do
    return ()

log :: Text -> IO ()
log message = do
    putStrLn (unpack message)
