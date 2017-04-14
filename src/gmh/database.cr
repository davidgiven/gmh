require "db"
require "sqlite3"

class Database
    @db : DB::Database

    def initialize(filename : String)
        @db = DB.open("sqlite3://" + filename)
        @db.exec("begin")
    end

    def initialize(flags : ParsedFlags)
        initialize(flags.get_string("database"))
    end

    def close
        @db.close
    end

    def commit
        @db.exec("commit")
        @db.exec("begin")
    end

    def get_var(name : String, default : String = "") : String
        s = @db.scalar("SELECT value FROM variables WHERE name = ?", name)
        if s.nil?
            default
        else
            s.as(String)
        end
    end

    def set_var(name : String, value : String) : Void
        @db.exec("INSERT OR REPLACE INTO variables (name, value) VALUES (?, ?)", name, value)
    end
end
