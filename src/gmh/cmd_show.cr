require "./flags"
require "./database"
require "./rfc2822"
require "io/memory"

class ShowFlags
    define_flags
end

def doShowCommand(globalFlags : GlobalFlags)
    flags = ShowFlags.new.parse(globalFlags.argv)

    db = Database.new(globalFlags)
    if flags.argv.size != 1
        raise UserException.new("syntax: show <uid>")
    end

    db.query("SELECT headers, body FROM messages INNER JOIN messageData
              ON messages.gmailId = messageData.docId
              WHERE messages.uid = ?", flags.argv[0].to_i) do |rs|
        headers = rs.read(String)
        headers = headers.gsub(/\s*\r\n\s+/, " ")
        body = rs.read(String)

        main = Rfc2822.parse_message(headers, body)
        puts main.subsections.size
    end

end
