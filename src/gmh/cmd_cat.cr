require "./flags"
require "./database"
require "./select"

class CatFlags
    define_flags
end

def doCatCommand(globalFlags : GlobalFlags)
    flags = CatFlags.new.parse(globalFlags.argv)

    db = Database.new(globalFlags)
    if flags.argv.size != 1
        raise UserException.new("syntax: cat <uid>")
    end

    db.query("SELECT headers, body FROM messages INNER JOIN messageData
              ON messages.gmailId = messageData.docId
              WHERE messages.uid = ?", flags.argv[0].to_i) do |rs|
        headers = rs.read(String)
        body = rs.read(String)

        puts headers
        puts "\n"
        puts body
    end
end
