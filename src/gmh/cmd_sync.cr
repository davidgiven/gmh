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

    capabilities = Set(String).new

    db = Database.new(globalFlags)
    imap = Imap.new("imap.gmail.com", 993) do |response|
        case response
            when CapabilitiesResponse
                capabilities = response.capabilities
            else
                puts "unknown response, ignoring"
        end
    end

    imap.login(db.get_var("username"), db.get_var("password"))
    imap.select("[Gmail]/All Mail")
end
