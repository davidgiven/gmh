require "socket"
require "openssl"
require "string_scanner"

class Imap
    @socket : OpenSSL::SSL::Socket
    @tag : Int32 = 0
    @capabilities = Set(String).new

    def initialize(host, port)
        socket = TCPSocket.new(host, port)
        context = OpenSSL::SSL::Context::Client.new

        @socket = OpenSSL::SSL::Socket::Client.new(socket, context)

        puts next_response
    end

    def capabilities
        @capabilities
    end

    private def put(line : String)
        puts (">" + line)
        @socket << line
    end

    private def get : String
        line = @socket.gets("\r\n", chomp=true)
        if line.nil?
            raise "Socket unexpectedly closed"
        end
        puts ("<" + line)
        line
    end

    private def next_response : Response
        line = get

        scanner = ResponseScanner.new(line)
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

    private def wait_for_response : Response
        while true
            response = next_response
            break if (response.tag != "*")

            case response
                when OKResponse
                    # do nothing

                when CapabilitiesResponse
                    @capabilities = response.capabilities
            end
        end
        response
    end

    private def new_tag : String
        @tag = @tag + 1
        "T" + @tag.to_s
    end

    private def wait_for_simple_command_response : OKResponse
        response = wait_for_response
        if !response.is_a?(OKResponse)
            raise BadResponseException.new(response)
        end
        response
    end

    def login(username : String, password : String)
        put(String.build do |io|
            io << new_tag << " login " << quoted(username) << " " << quoted(password) << "\r\n"
        end)
        wait_for_simple_command_response
    end

    def select(mailbox : String)
        put(String.build do |io|
            io << new_tag << " select " << quoted(mailbox) << "\r\n"
        end)
        wait_for_simple_command_response
    end
end

private def quoted(s : String)
    '"' + s.gsub(/"/, "\\\"") + '"'
end

class UnmatchedException < Exception
    def initialize(s)
        super("cannot match '#{s}...'")
    end
end

class BadResponseException < Exception
    def initialize(r)
        super("bad response from IMAP server: #{r}")
    end
end

abstract class Response
    @tag : String
    @line : String

    def initialize(tag, line, scanner)
        @tag = tag
        @line = line
        parse(scanner)
    end

    abstract def parse(scanner)

    def tag
        @tag
    end

    def line
        @line
    end

    def to_s(io)
        io << self.class << "{" << line << "}"
    end
end

class TrailingResponse < Response
    @trailing : String = ""

    def parse(scanner)
        @trailing = scanner.expect_trailing
    end

    def trailing
        @trailing
    end
end

class OKResponse < TrailingResponse
end

class NOResponse < TrailingResponse
end

class CapabilitiesResponse < Response
    @capabilities = Set(String).new

    def parse(scanner)
        while !scanner.eos?
            @capabilities.add(scanner.expect_atom)
        end
    end

    def capabilities
        @capabilities
    end
end

class ResponseScanner < StringScanner
    def expect(regex)
        s = scan(regex)
        if s.nil?
            raise UnmatchedException.new(self.peek(20))
        end
        s
    end

    def expect_ws : Void
        expect(/ */)
    end

    def expect_tag : String
        tag = expect(/\*|[A-Za-z0-9]+/)
        expect_ws
        tag.as(String)
    end

    def expect_atom : String
        atom = expect(/[A-Za-z0-9=\\$-]+/)
        expect_ws
        atom.as(String)
    end

    def expect_atom(want) : Void
        got = expect_atom
        if (got != want)
            raise UnmatchedException.new(got)
        end
    end

    def expect_trailing : String
        scan(/.*/).as(String)
    end
end
