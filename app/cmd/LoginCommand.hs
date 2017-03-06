module LoginCommand where
import GlobalOptions

run :: GlobalOptions -> [String] -> IO ()
run globalOptions argv = do
    print "run"
