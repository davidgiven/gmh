require "./gmh/imap"
require "./gmh/database"
require "./gmh/flags"

globalFlagDescription = Flagset{
    "database" => Flags::StringFlag.new(["-d", "--database"], "Path to mail database", "/home/dg/.gmh.sqlite")
}

globalOptions = parseFlags(globalFlagDescription, ARGV)

puts "connecting"
db = Database.new("/home/dg/.gmh.sqlite")
imap = Imap.new("imap.gmail.com", 993, ->(r : Response){})
imap.login(db.get_var("username"), db.get_var("password"))

if !imap.capabilities.includes?("X-GM-EXT-1")
    puts "This isn't a Gmail server!"
    exit 0
end

imap.select("[Gmail]/All Mail")
puts "done"
