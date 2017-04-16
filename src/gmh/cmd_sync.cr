require "./database"
require "./imap"
require "./flags"

class SyncFlags
    define_flags({
        force_uid_refresh: BoolFlag.new(
                ["--force-uid-refresh"],
                "Force a UID resync (useful if the database gets confused)",
                false),
        batch_size: IntFlag.new(
                ["--batch-size"],
                "Fetch (and commit) this many messages in a single batch",
                500)
    })
end

class MessageSkeleton
    @gmail_id : Int64?
    @uid : Int64?
    @flags : Set(String)?
    @labels : Set(String)?

    private def to_i64(s : String?) : Int64?
        if s.nil?
            nil
        else
            s.to_i64
        end
    end

    private def to_set(a : Array(ImapElement)?) : Set(String)?
        if a.nil?
            nil
        else
            s = Set(String).new
            a.each do |e|
                s << e.as(String)
            end
            s
        end
    end

    def initialize(msg : FetchResponse)
        @gmail_id = to_i64(msg.attr["X-GM-MSGID"].as(String?))
        @uid = to_i64(msg.attr["UID"].as(String?))
        @flags = to_set(msg.attr["FLAGS"].as(Array(ImapElement)?))
        @labels = to_set(msg.attr["X-GM-LABELS"].as(Array(ImapElement)?))
    end

    def write_to(db : Database)
        if @gmail_id.nil?
            return
        end

        db.add_message(@gmail_id.as(Int64))
        if !@uid.nil?
            db.set_message_uid(@gmail_id.as(Int64), @uid.as(Int64))
        end
        if !@flags.nil?
            db.set_message_flags(@gmail_id.as(Int64), @flags.as(Set(String)))
        end
        if !@labels.nil?
            db.set_message_labels(@gmail_id.as(Int64), @labels.as(Set(String)))
        end
    end
end

def doSyncCommand(globalFlags : GlobalFlags)
    flags = SyncFlags.new.parse(globalFlags.argv)
    if flags.argv.size != 0
        raise UserException.new("syntax: sync [<flags>...]")
    end

    db = Database.new(globalFlags)

    capabilities = Set(String).new
    uid_validity = 0_i64
    highest_modseq = 0_i64
    old_uid_validity = db.get_var("uidvalidity", "0").to_i64
    old_highest_modseq = db.get_var("modseq", "0").to_i64

    imap = Imap.new("imap.gmail.com", 993, globalFlags) do |response|
        case response
            when CapabilitiesResponse
                capabilities = response.capabilities
            when UidValidityOKResponse
                uid_validity = response.value
            when HighestModSeqOKResponse
                highest_modseq = response.value
            when FlagsResponse
                # ignore silently
            when FetchResponse
                MessageSkeleton.new(response).write_to(db)
            else
                puts "unknown response #{response}, ignoring"
        end
    end

    imap.login(db.get_var("username"), db.get_var("password"))
    imap.select("[Gmail]/All Mail")

    if flags.force_uid_refresh || (uid_validity != old_uid_validity)
        puts("refreshing UID map")
        imap.command("FETCH 1:* (UID X-GM-MSGID)")
        db.set_var("uidvalidity", uid_validity.to_s)
        db.commit
    end

    puts("fetching message updates from #{old_highest_modseq} to #{highest_modseq}")
    imap.command("FETCH 1:* (UID X-GM-MSGID X-GM-LABELS FLAGS) (CHANGEDSINCE #{old_highest_modseq})")
    db.set_var("modseq", highest_modseq.to_s)
    db.commit
end
