require "./database"
require "./flags"

def doLoginCommand(globalFlags : ParsedFlags)
    flags = Flagset.new.parse(globalFlags.rest)
    if flags.rest.size != 2
        raise UserException.new("syntax: login <username> <password>")
    end

    username = flags.rest[0]
    password = flags.rest[1]

    db = Database.new(globalFlags)
    db.set_var("username", username)
    db.set_var("password", password)
    db.commit
end
