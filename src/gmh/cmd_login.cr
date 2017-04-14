require "./database"
require "./flags"

class LoginFlags
    define_flags
end

def doLoginCommand(globalFlags : GlobalFlags)
    flags = LoginFlags.new.parse(globalFlags.argv)
    if flags.argv.size != 2
        raise UserException.new("syntax: login <username> <password>")
    end

    username = flags.argv[0]
    password = flags.argv[1]

    db = Database.new(globalFlags)
    db.set_var("username", username)
    db.set_var("password", password)
    db.commit
end
