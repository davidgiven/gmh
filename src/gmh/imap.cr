require "socket"
require "openssl"
require "string_scanner"

class Imap
    @socket : OpenSSL::SSL::Socket
    @tag : Int32 = 0

    def initialize(host, port)
        socket = TCPSocket.new(host, port)
        context = OpenSSL::SSL::Context::Client.new

        @socket = OpenSSL::SSL::Socket::Client.new(socket, context)

        puts next_response
    end

    private def put(line : String)
        puts (">" + line)
        @socket << line
    end

    private def get : String
        line = @socket.gets("\r\n")
        if line.nil?
            raise "Socket unexpectedly closed"
        end
        puts ("<" + line)
        line
    end

    private def next_response : Response
        line = get

        scanner = ResponseScanner.new(line)
        scanner.expect_untagged
        scanner.expect_atom("OK")
        return OKResponse.new(line, scanner.expect_trailing)
    end

    private def new_tag : String
        @tag = @tag + 1
        "T" + @tag.to_s
    end

    def login(username : String, password : String)
        tag = new_tag
        @socket << tag << " login " << quoted(username) << " " << quoted(password) << "\r\n"

        response = next_response
        puts response
    end

    def select(mailbox : String)
        tag = new_tag
        @socket << tag << " select " << quoted(mailbox) << "\r\n"

        response = next_response
        puts response
    end
end

private def quoted(s : String)
    '"' + s.gsub(/"/, "\\\"") + '"'
end

class Response
    @line : String

    def initialize(line)
        @line = line
    end

    def line
        @line
    end

    def to_s(io)
        io << self.class << "{" << line << "}"
    end
end

class OKResponse < Response
    @trailing : String

    def initialize(line, trailing)
        super(line)
        @trailing = trailing
    end

    def trailing
        @trailing
    end
end

class UnmatchedException < Exception
    def initialise(s)
        super("cannot match '#{s}...'")
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
        scan(/ */)
    end

    def expect_untagged : Void
        scan(/\*/)
        expect_ws
    end

    def expect_atom : String
        atom = scan(/[A-Z]*/)
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
