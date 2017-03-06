{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import qualified Flags as Flags
import qualified System.Environment as Environment
import qualified LoginCommand as LoginCommand
import GlobalOptions

main :: IO ()
main =
    do
        argv <- Environment.getArgs
        (Flags.ParsedFlags globalOptions rest) <-
            return (Flags.parse defaultGlobalOptions argv globalOptionsDescription)
        doCommand globalOptions rest
    where
        doCommand :: GlobalOptions -> [String] -> IO ()
        doCommand globalOptions [] =
            error "no command specified (try --help)"

        doCommand globalOptions ("login":rest) = LoginCommand.run globalOptions rest

        doCommand globalOptions (command:rest) =
            error ("command '" ++ command ++ "' not recognised (try --help)")
