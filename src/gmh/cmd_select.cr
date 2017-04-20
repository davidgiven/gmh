require "./flags"
require "./database"
require "./select"

class SelectFlags
    define_flags
end

def doSelectCommand(globalFlags : GlobalFlags)
    flags = SelectFlags.new.parse(globalFlags.argv)

    db = Database.new(globalFlags)
    if flags.argv.size > 0
        MessageSelector.select(db, flags.argv)
    end
    puts "Selected #{db.get_selected_message_count} message(s)"
    db.commit
end
