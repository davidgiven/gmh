require "./imap_scanner"

private module ImapResponseParser
    def parse(line : String) : ImapResponse
        scanner = ImapResponseScanner.new(line)
        tag = scanner.expect_tag
        atom = scanner.expect_atom
        case atom
            when "OK"
                return OKResponse.new(tag, line, scanner)
            when "NO"
                return NOResponse.new(tag, line, scanner)
            when "CAPABILITY"
                return CapabilitiesResponse.new(tag, line, scanner)
            else
                raise UnmatchedException.new(atom)
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

