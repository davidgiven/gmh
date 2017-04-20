require "./gmh/flags"
require "./gmh/cmd_init"
require "./gmh/cmd_login"
require "./gmh/cmd_sync"
require "./gmh/cmd_select"
require "./gmh/globals"

class GlobalFlags
    define_flags ({
        database: StringFlag.new(
            ["-d", "--database"],
            "Path to mail database",
            "/home/dg/.gmh.sqlite"),
        trace_imap: BoolFlag.new(
            ["--trace-imap"],
            "Dump all IMAP requests/responses to stdout (warning: very spammy)",
            false)
    })
end

def main
    flags = GlobalFlags.new.parse(ARGV)

    command = flags.argv[0]?
    flags.argv.shift
    case command
        when nil
            raise UserException.new("no command specified (try 'help')")

        when "init"
            doInitCommand(flags)
        when "login"
            doLoginCommand(flags)
        when "sync"
            doSyncCommand(flags)
        when "select"
            doSelectCommand(flags)

        else
            raise UserException.new("invalid command '%s' (try 'help')" % command)
    end
end

begin
    main
rescue e : UserException
    puts "error: %s" % e.message
end
