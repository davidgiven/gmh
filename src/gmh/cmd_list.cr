require "./flags"
require "./database"
require "./select"
require "./curses"

class ListFlags
    define_flags({
        messages: BoolFlag.new(
                ["--messages", "-m"],
                "Show individual messages, not just threads",
                false),
    })
end

private def approximate_date(seconds_since_epoch : Int64)
    t = Time.epoch(seconds_since_epoch)
    age = Time.now - t
    if age.total_days < 7.0
        format = "%a %H:%M "
    else
        format = "%F"
    end

    return t.to_s(format)
end

private abstract class Lister
    @db : Database
    @max_uid : Int32
    @uid_width : Int32

    def initialize(db : Database)
        @db = db

        @max_uid = 0
        @db.query("SELECT max(uid) FROM messages") do |rs|
            @max_uid = rs.read(Int32)
        end
        @uid_width = LibM.log10_f64(@max_uid.to_f).to_i + 1
    end

    def show_messages : Void
        @db.query("SELECT uid, gmailId, date, subject FROM messages INNER JOIN messageData
                   ON messages.gmailId = messageData.docId
                   WHERE uid IN (#{uidset})") do |rs|
            show_message(rs)
        end
    end

    def show_message(rs : SQLite3::ResultSet) : Void
        uid = rs.read(Int32)
        gmail_id = rs.read(Int64)
        date = rs.read(Int64)
        subject = rs.read(String)

        from_email = "?"
        from_name = "?"
        @db.query("SELECT email, name FROM addresses WHERE addressId IN
                   (SELECT addressId FROM addressMap WHERE
                   gmailId = ? AND kind = ?)", gmail_id, AddressKind::FROM.value) do |rs|
            from_email = rs.read(String)
            from_name = rs.read(String)
        end

        flags = self.flags(gmail_id)
        meta_width = @uid_width + 1 + 10 + 1 + flags.size + 1 + from_name.size + 1
        subject_width = Termcap.width - meta_width
        s = String::Builder.new
        s << "%*d " % {@uid_width, uid}
        s << approximate_date(date)
        s << " "
        s << flags
        s << " "
        s << from_name
        s << " "
        s << Termcap["md"]
        s << ((subject_width > 0) ? subject[0, subject_width] : "")
        s << Termcap["me"]

        LibNcursesw.putp(s.to_s)
        LibNcursesw.putp("\n")
    end

    abstract def uidset : String
    abstract def flags(gmail_id : Int64) : String
end

private class ThreadLister < Lister
    def uidset : String
        "SELECT MIN(uid) FROM messages WHERE threadId IN
         (SELECT DISTINCT threadId FROM selected INNER JOIN messages
              WHERE selected.gmailId = messages.gmailId)
         GROUP BY threadId
         ORDER BY date ASC"
    end

    def flags(gmail_id : Int64) : String
        num_messages = 0
        read_messages = 0

        thread_id = -1_i64
        @db.query("SELECT threadId FROM messages WHERE gmailId = ?", gmail_id) do |rs|
            thread_id = rs.read(Int64)
        end

        @db.query("SELECT count(*) FROM messages WHERE threadId = ?", thread_id) do |rs|
            num_messages = rs.read(Int32)
        end
        read_messages = 0
        @db.query("SELECT count(gmailId) FROM messages
                   WHERE threadId = ?
                   AND EXISTS (SELECT * FROM flagMapFused
                        WHERE gmailId = messages.gmailId AND flagId =
                     (SELECT flagId FROM flags WHERE name = '\\Seen'))", thread_id) do |rs|
            read_messages = rs.read(Int32)
        end

        unread_messages = num_messages - read_messages
        "%d/%d" % {unread_messages, num_messages}
    end
end

private class MessageLister < Lister
    def uidset : String
        "SELECT uid FROM selected INNER JOIN messages
         WHERE selected.gmailId = messages.gmailId
         ORDER BY date ASC"
    end

    def flags(gmail_id : Int64) : String
        replied = false
        unseen = true

        @db.query("SELECT name FROM flags WHERE flagId IN
                   (SELECT flagId FROM flagMapFused WHERE gmailId = ?)", gmail_id) do |rs|
            case rs.read(String)
                when "\\Seen"
                    unseen = false
                when "\\Answered"
                    replied = true
            end
        end

        (unseen ? "U" : " ") + (replied ? "R" : " ")
    end
end

def doListCommand(globalFlags : GlobalFlags)
    flags = ListFlags.new.parse(globalFlags.argv)

    db = Database.new(globalFlags)
    if flags.argv.size > 0
        MessageSelector.select(db, flags.argv)
    end

    if flags.messages
        MessageLister.new(db).show_messages
    else
        ThreadLister.new(db).show_messages
    end
end
