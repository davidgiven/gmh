require "db"
require "sqlite3"

class Database
    @db : DB::Database

    def initialize(filename : String)
        @db = DB.open("sqlite3://" + filename)
    end

    def get_var(name : String, default : String = "") : String
        s = @db.scalar("SELECT value FROM variables WHERE name = ?", name)
        if s.nil?
            default
        else
            s.as(String)
        end
    end
end
