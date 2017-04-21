require "db"
require "sqlite3"

enum AddressKind
    FROM,
    TO,
    CC,
    BCC,
    REPLY_TO,
    SENDER
end

class Database
    @db : DB::Database

    def initialize(filename : String)
        @db = DB.open("sqlite3://" + filename)
        @db.exec("begin")
    end

    def initialize(flags : GlobalFlags)
        initialize(flags.database)
    end

    def close
        @db.close
    end

    def commit
        @db.exec("commit")
        @db.exec("begin")
    end

    def exec(sql : String, *args)
        @db.exec(sql, *args)
    end

    def query(sql : String, *args, &block)
        @db.query(sql, *args) do |rs|
            rs.each do
                yield rs
            end
        end
    end

    def get_var(name : String, default : String = "") : String
        @db.query("SELECT value FROM variables WHERE name = ?", name) do |rs|
            rs.each do
                return rs.read.as(String)
            end
        end

        default
    end

    def set_var(name : String, value : String) : Void
        @db.exec("INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)", name, value)
    end

    def add_message(gmail_id : Int64) : Void
        @db.exec("INSERT OR IGNORE INTO messages (gmailId) VALUES (?)", gmail_id)
        @db.exec("INSERT OR IGNORE INTO messageData (docId, subject, body) VALUES (?, '', '')", gmail_id)
    end

    def set_message_uid(gmail_id : Int64, uid : Int64) : Void
        @db.exec("UPDATE messages SET uid = ? WHERE gmailId = ?", uid, gmail_id)
    end

    def set_message_thread(gmail_id : Int64, thread_id : Int64) : Void
        @db.exec("UPDATE messages SET threadId = ? WHERE gmailId = ?", thread_id, gmail_id)
    end

    def set_message_flags(gmail_id : Int64, flags : Set(String)) : Void
        @db.exec("DELETE FROM flagMap WHERE gmailId = ?", gmail_id)
        flags.each do |flag|
            @db.exec("INSERT OR IGNORE INTO flags (name) VALUES (?)", flag)
            @db.exec("INSERT INTO flagMap (gmailId, flagId) VALUES (" +
                "?, (SELECT flagId FROM flags WHERE name = ?))",
                gmail_id, flag)
        end
    end

    def set_message_labels(gmail_id : Int64, labels : Set(String)) : Void
        @db.exec("DELETE FROM labelMap WHERE gmailId = ?", gmail_id)
        labels.each do |label|
            @db.exec("INSERT OR IGNORE INTO labels (name) VALUES (?)", label)
            @db.exec("INSERT INTO labelMap (gmailId, labelId) VALUES (" +
                "?, (SELECT labelId FROM labels WHERE name = ?))",
                gmail_id, label)
        end
    end

    def get_non_downloaded_uids(max : Int32) : Set(Int64)
        uids = Set(Int64).new
        @db.query(
            <<-SQL
                SELECT uid FROM messages WHERE
                    downloaded = 0 AND uid IS NOT NULL
                    ORDER BY date DESC
                    LIMIT ?
            SQL, max
        ) do |rs|
            uids = Set(Int64).new
            rs.each do
                uids << rs.read.as(Int64)
            end
            return uids
        end
    end

    private def time_to_epoch(time : Time?) : Int64?
        if time
            time.epoch
        else
            nil
        end
    end

    def set_message_addresses(gmail_id : Int64, addresses : Set(ImapEmail), kind : AddressKind)
        @db.exec("DELETE FROM addressMap WHERE gmailId = ? AND kind = ?",
            gmail_id, kind.value)

        addresses.each do | address |
            @db.exec("INSERT OR IGNORE INTO addresses (email, name) VALUES (?, ?)",
                address.email, address.realname)
            @db.exec(<<-SQL
                INSERT INTO addressMap (gmailId, addressId, kind)
                    VALUES (?, (SELECT addressId FROM addresses WHERE email = ?), ?)
            SQL, gmail_id, address.email, kind.value)
        end
    end

    def set_message_envelope(gmail_id : Int64, envelope : ImapEnvelope) : Void
        @db.exec("UPDATE messages SET messageId = ? WHERE gmailId = ?",
            envelope.message_id, gmail_id)
        @db.exec("UPDATE messageData SET subject = ? WHERE docId = ?",
            envelope.subject, gmail_id)
        set_message_addresses(gmail_id, envelope.from, AddressKind::FROM)
        set_message_addresses(gmail_id, envelope.sender, AddressKind::SENDER)
        set_message_addresses(gmail_id, envelope.reply_to, AddressKind::REPLY_TO)
        set_message_addresses(gmail_id, envelope.to, AddressKind::TO)
        set_message_addresses(gmail_id, envelope.cc, AddressKind::CC)
        set_message_addresses(gmail_id, envelope.bcc, AddressKind::BCC)
    end

    def set_message_date(gmail_id : Int64, date : Time) : Void
        @db.exec("UPDATE messages SET date = ? WHERE gmailId = ?",
            time_to_epoch(date), gmail_id)
    end

    def set_message_headers(gmail_id : Int64, headers : String) : Void
        @db.exec("UPDATE messages SET headers = ? WHERE gmailId = ?", headers, gmail_id)
    end

    def set_message_body(gmail_id : Int64, body : String) : Void
        @db.exec("UPDATE messages SET downloaded = 1 WHERE gmailId = ?", gmail_id)
        @db.exec("UPDATE messageData SET body = ? WHERE docId = ?",
            body, gmail_id)
    end

    def get_selected_message_count : Int32
        return @db.scalar("SELECT COUNT(*) FROM selected").as(Int64).to_i
    end
end
