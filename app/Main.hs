module Main where
import qualified Flags as Flags
import qualified System.Environment as Environment
import qualified LoginCommand as LoginCommand
import qualified InitCommand as InitCommand
import qualified SyncCommand as SyncCommand
import GlobalOptions
import Data.Text
import Data.Monoid

main :: IO ()
main =
    do
        argvStrings <- Environment.getArgs
        let argv = Prelude.map pack argvStrings
        (Flags.ParsedFlags globalOptions rest) <-
            return (Flags.parse defaultGlobalOptions argv globalOptionsDescription)
        doCommand globalOptions rest
    where
        doCommand :: GlobalOptions -> [Text] -> IO ()
        doCommand globalOptions [] =
            error "no command specified (try --help)"

        doCommand globalOptions (command:rest) =
            (whichCommand command) globalOptions rest

        whichCommand :: Text -> (GlobalOptions -> [Text] -> IO ())
        whichCommand "login" = LoginCommand.run
        whichCommand "init" = InitCommand.run
        whichCommand "sync" = SyncCommand.run
        whichCommand command =
            error $ unpack ("command '" <> command <> "' not recognised (try --help)")
