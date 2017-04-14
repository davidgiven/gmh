require "./database"
require "./imap"
require "./flags"

class SyncFlags
    define_flags({
        force_uid_refresh: BoolFlag.new(
                ["--force-uid-refresh"],
                "Force a UID resync (useful if the database gets confused)",
                false),
        batch_size: IntFlag.new(
                ["--batch-size"],
                "Fetch (and commit) this many messages in a single batch",
                500)
    })
end

def doSyncCommand(globalFlags : GlobalFlags)
    flags = SyncFlags.new.parse(globalFlags.argv)
    if flags.argv.size != 0
        raise UserException.new("syntax: sync [<flags>...]")
    end
end
