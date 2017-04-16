require "db"
require "sqlite3"

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

    def exec(sql : String)
        @db.exec(sql)
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
    end

    def set_message_uid(gmail_id : Int64, uid : Int64) : Void
        @db.exec("UPDATE messages SET uid = ? WHERE gmailId = ?", uid, gmail_id)
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
end
