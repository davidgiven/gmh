require "string_scanner"
require "./imap_scanner"
require "./rfc2822"

private macro try_parsing(expr)
    old_offset = scanner.offset
    begin
        return {{ expr }}
    rescue UnmatchedException
    end
    scanner.offset = old_offset
end

private module ImapResponseParser
    def parse(line : String) : ImapResponse
        scanner = ImapResponseScanner.new(line)
        tag = scanner.expect_tag
        atom = scanner.expect_atom
        case atom
            when "OK"
                try_parsing UidValidityOKResponse.new(tag, line, scanner)
                try_parsing HighestModSeqOKResponse.new(tag, line, scanner)
                try_parsing PermanentFlagsOKResponse.new(tag, line, scanner)
                try_parsing UidNextOKResponse.new(tag, line, scanner)
                try_parsing ReadWriteOKResponse.new(tag, line, scanner)
                if scanner.at_open_sq_bracket
                    raise UnmatchedException.new(scanner.peek(20))
                end
                return OKResponse.new(tag, line, scanner)
            when "NO"
                return NOResponse.new(tag, line, scanner)
            when "CAPABILITY"
                return CapabilitiesResponse.new(tag, line, scanner)
            when "FLAGS"
                return FlagsResponse.new(tag, line, scanner)
            else
                try_parsing ExistsResponse.new(tag, line, scanner, atom)
                try_parsing RecentResponse.new(tag, line, scanner, atom)
                FetchResponse.new(tag, line, scanner)
        end
    end
end

abstract class ImapResponse
    extend ImapResponseParser

    getter tag : String
    getter line : String

    def initialize(tag, line, scanner)
        @tag = tag
        @line = line
        parse(scanner)
    end

    abstract def parse(scanner)

    def to_s(io)
        io << self.class << "{" << line << "}"
    end
end

class TrailingResponse < ImapResponse
    getter trailing : String = ""

    def parse(scanner)
        @trailing = scanner.expect_trailing
    end
end

class OKResponse < TrailingResponse
end

class NOResponse < TrailingResponse
end

class CapabilitiesResponse < ImapResponse
    getter capabilities = Set(String).new

    def parse(scanner)
        while !scanner.eos?
            @capabilities.add(scanner.expect_atom)
        end
    end
end

class FlagsResponse < ImapResponse
    getter flags = Set(String).new

    def parse(scanner)
        scanner.expect_element.as(Array(ImapElement)).each do |flag|
            @flags << flag.as(String)
        end
        scanner.expect_eol
    end
end

abstract class OKWithAtomResponse < OKResponse
    def parse(scanner)
        scanner.expect_open_sq_bracket
        scanner.expect_atom(atom)
        scanner.expect_close_sq_bracket
        super(scanner)
    end

    abstract def atom : String
end

class ReadWriteOKResponse < OKWithAtomResponse
    def atom
        "READ-WRITE"
    end
end

abstract class OKWithAtomAndElementResponse < OKResponse
    def parse(scanner)
        scanner.expect_open_sq_bracket
        scanner.expect_atom(atom)
        set_value(scanner.expect_element)
        scanner.expect_close_sq_bracket
        super(scanner)
    end

    abstract def atom : String
    abstract def set_value(element : ImapElement) : Void
end

abstract class OKWithAtomAndIntegerResponse < OKWithAtomAndElementResponse
    getter value = 0

    abstract def atom : String

    def set_value(element : ImapElement) : Void
        @value = element.as(String).to_i
    end
end

class UidValidityOKResponse < OKWithAtomAndIntegerResponse
    def atom
        "UIDVALIDITY"
    end
end

class HighestModSeqOKResponse < OKWithAtomAndIntegerResponse
    def atom
        "HIGHESTMODSEQ"
    end
end

class UidNextOKResponse < OKWithAtomAndIntegerResponse
    def atom
        "UIDNEXT"
    end
end

class PermanentFlagsOKResponse < OKResponse
    def parse(scanner)
        scanner.expect_open_sq_bracket
        scanner.expect_atom("PERMANENTFLAGS")
        scanner.expect_element
        scanner.expect_close_sq_bracket
        super(scanner)
    end
end

abstract class NumericResponse < ImapResponse
    getter value = 0_i64

    def initialize(tag, line, scanner, atom)
        super(tag, line, scanner)
        @value = atom.to_i64
    end
end

class ExistsResponse < NumericResponse
    def parse(scanner)
        scanner.expect_atom("EXISTS")
        scanner.expect_eol
    end
end

class RecentResponse < NumericResponse
    def parse(scanner)
        scanner.expect_atom("RECENT")
        scanner.expect_eol
    end
end

class FetchResponse < ImapResponse
    getter attr = Hash(String, ImapElement).new
    getter flags = Set(String).new
    getter body : String?

    def parse(scanner)
        scanner.expect_atom("FETCH")
        scanner.expect_open_paren
        while !scanner.at_close_paren
            key = scanner.expect_atom
            if scanner.at_open_sq_bracket
                scanner.expect_open_sq_bracket
                scanner.expect_close_sq_bracket
                key = key + "[]"
            end
            value = scanner.expect_element
            attr[key] = value
        end
        scanner.expect_close_paren
        scanner.expect_eol
    end
end

struct ImapEmail
    getter realname : String
    getter email : String

    def initialize(imap : Array(ImapElement))
        @realname = Rfc2822.parse_encoded(imap[0].as(String))
        @email = imap[2].as(String) + "@" + imap[3].as(String)

        if @realname == "NIL"
            @realname = @email
        end
    end

    def initialize(realname : String, email : String)
        @realname = realname
        @email = email
    end

    def to_s(io)
        if @realname == @email
            io << @email
        else
            io << @realname << " <" << @email << ">"
        end
    end
end

class ImapEnvelope
    getter subject = ""
    getter from = Set(ImapEmail).new
    getter sender = Set(ImapEmail).new
    getter reply_to = Set(ImapEmail).new
    getter to = Set(ImapEmail).new
    getter cc = Set(ImapEmail).new
    getter bcc = Set(ImapEmail).new
    getter in_reply_to : String?
    getter message_id : String

    private def imap_to_emails(imap : ImapElement) : Set(ImapEmail)
        s = Set(ImapEmail).new
        if imap != "NIL"
            imap.as(Array(ImapElement)).each do |email|
                s << ImapEmail.new(email.as(Array(ImapElement)))
            end
        end
        s
    end

    private def imap_to_s(imap : ImapElement) : String?
        if imap = "NIL"
            nil
        else
            imap.as(String)
        end
    end

    def initialize(imap : Array(ImapElement))
        @subject = Rfc2822.parse_encoded(imap[1].as(String))
        if @subject == "NIL"
            @subject = ""
        end
        @from = imap_to_emails(imap[2])
        @sender = imap_to_emails(imap[3])
        @reply_to = imap_to_emails(imap[4])
        @to = imap_to_emails(imap[5])
        @cc = imap_to_emails(imap[6])
        @bcc = imap_to_emails(imap[7])
        @in_reply_to = imap_to_s(imap[8])
        @message_id = imap[9].as(String)
    end
end

