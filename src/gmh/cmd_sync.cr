require "./database"
require "./imap"
require "./flags"
require "./progress"

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
    @thread_id : Int64?
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
        @thread_id = to_i64(msg.attr["X-GM-THRID"].as(String?))
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
    approximate_message_count = 0_i64
    old_uid_validity = db.get_var("uidvalidity", "0").to_i64
    old_highest_modseq = db.get_var("modseq", "0").to_i64

    imap = Imap.new("imap.gmail.com", 993, globalFlags)

    handler = ->(response : ImapResponse) {
        case response
            when CapabilitiesResponse
                capabilities = response.capabilities
            when UidValidityOKResponse
                uid_validity = response.value
            when HighestModSeqOKResponse
                highest_modseq = response.value
            when ExistsResponse
                approximate_message_count = response.value
            when FlagsResponse | PermanentFlagsOKResponse | RecentResponse
                # ignore silently
            when FetchResponse
                MessageSkeleton.new(response).write_to(db)
            else
                puts "unknown response #{response}, ignoring"
        end
    }

    imap.login(db.get_var("username"), db.get_var("password"), &handler)
    imap.select("[Gmail]/All Mail", &handler)

    if flags.force_uid_refresh || (uid_validity != old_uid_validity)
        puts("refreshing UID map")
        ProgressBar.count(approximate_message_count.to_i32) do |pb|
            imap.command("FETCH 1:* (UID X-GM-MSGID)") do |r|
                pb.next
                handler.call(r)
            end
        end
        db.set_var("uidvalidity", uid_validity.to_s)
        db.commit
    end

    puts("fetching message updates from #{old_highest_modseq} to #{highest_modseq}")
    imap.command("FETCH 1:* (UID X-GM-MSGID X-GM-LABELS FLAGS) (CHANGEDSINCE #{old_highest_modseq})", &handler)
    db.set_var("modseq", highest_modseq.to_s)
    db.commit

    puts("fetching message bodies (this bit is ^C-able)")
    non_downloaded_uids_count = db.get_non_downloaded_uids(Int32::MAX).size
    ProgressBar.count(non_downloaded_uids_count) do |pb|
        while true
            uid_batch = db.get_non_downloaded_uids(flags.batch_size)
            if uid_batch.size == 0
                break
            end

            cmd = String::Builder.new
            cmd << "FETCH "
            first = true
            uid_batch.each do |uid|
                if !first
                    cmd << ','
                end
                first = false
                cmd << uid
            end

            cmd << " (X-GM-MSGID X-GM-THRID ENVELOPE BODY.PEEK[])"
            imap.command(cmd.to_s) do |r|
                handler.call(r)
                if r.is_a?(FetchResponse)
                    pb.next
                end
            end

            db.commit
        end
    end
    puts non_downloaded_uids_count
end
