require "./gmh/flags"
require "./gmh/cmd_init"
require "./gmh/cmd_login"
require "./gmh/cmd_sync"
require "./gmh/globals"

class GlobalFlags
    define_flags ({
        database: StringFlag.new(
            ["-d", "--database"],
            "Path to mail database",
            "/home/dg/.gmh.sqlite")
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

        else
            raise UserException.new("invalid command '%s' (try 'help')" % command)
    end

#    puts "connecting"
#    db = Database.new("/home/dg/.gmh.sqlite")
#    imap = Imap.new("imap.gmail.com", 993, ->(r : Response){})
#    imap.login(db.get_var("username"), db.get_var("password"))
#
#    if !imap.capabilities.includes?("X-GM-EXT-1")
#        puts "This isn't a Gmail server!"
#        exit 0
#    end
#
#    imap.select("[Gmail]/All Mail")
#    puts "done"
end

begin
    main
rescue e : UserException
    puts "error: %s" % e.message
end
