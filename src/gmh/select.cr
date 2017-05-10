require "./database"

module MessageSelector
    extend self

    class BadSelectionSpecException < Exception
    end

    private def exec_condition(db : Database, condition : String, &block) : Void
        if condition == "all:"
            yield "SELECT gmailId FROM messages", [] of String
        elsif condition == "none:"
            yield "SELECT NULL LIMIT 0", [] of String
        elsif m = /^label:(.*)$/i.match(condition)
            yield "SELECT gmailId FROM labelMap WHERE labelId =
                (SELECT labelId FROM labels WHERE name = ?)", [m[1]]
        elsif m = /^flag:(.*)$/i.match(condition)
            yield "SELECT gmailId FROM flagMapFused WHERE flagId =
                (SELECT flagId FROM flags WHERE name = ?)", [m[1]]
        elsif m = /^subject:(.*)$/i.match(condition)
            yield "SELECT docId FROM messageData WHERE subject MATCH ?", [m[1]]
        elsif m = /^body:(.*)$/i.match(condition)
            yield "SELECT docId FROM messageData WHERE body MATCH ?", [m[1]]
        elsif m = /^uid:(.*)$/i.match(condition)
            yield "SELECT gmailId FROM messages WHERE uid = ?", [m[1].to_i]
        elsif m = /^thread:(.*)$/i.match(condition)
            yield "SELECT gmailId FROM messages WHERE threadId =
                     (SELECT threadId FROM messages WHERE uid = ?)", [m[1].to_i]
        else
            raise BadSelectionSpecException.new("unrecognised condition '#{condition}'")
        end
    end

    private def do_select(db : Database, argv : Array(String)) : Void
        if argv.size == 0
            raise BadSelectionSpecException.new("you must select something (try all: or label:\Inbox")
        end

        if /^(or|and)$/i !~ argv[0]
            db.exec("DELETE FROM selected")
            argv.unshift("OR")
        end

        while argv.size > 0
            operator = argv.shift
            if argv.size == 0
                raise BadSelectionSpecException.new("missing operator before '#{operator}' --- did you want an OR or AND?")
            end
            arg = argv.shift
            if /^not$/i =~ arg
                operator += " not"
                arg = argv.shift
            end

            if /^or$/i =~ operator
                exec_condition(db, arg) do |sql, args|
                    db.exec("INSERT INTO selected (gmailId) #{sql}", args)
                end
            elsif /^or not$/i =~ operator
                exec_condition(db, arg) do |sql, args|
                    db.exec("INSERT INTO selected (gmailId)
                        SELECT gmailId FROM messages WHERE gmailId NOT IN (#{sql})", args)
                end
            elsif /^and$/i =~ operator
                exec_condition(db, arg) do |sql, args|
                    db.exec("DELETE FROM selected WHERE gmailId NOT IN (#{sql})", args)
                end
            elsif /^and not$/i =~ operator
                exec_condition(db, arg) do |sql, args|
                    db.exec("DELETE FROM selected WHERE gmailId IN (#{sql})", args)
                end
            else
                raise BadSelectionSpecException.new("unknown operator '#{operator}' --- try OR or AND")
            end
        end
    end

    def select(db : Database, argv : Array(String)) : Void
        do_select(db, argv)
    end
end
