require "socket"
require "openssl"
require "string_scanner"
require "./imap_responses"
require "./globals"

class Imap
    @socket : OpenSSL::SSL::Socket
    @flags : GlobalFlags
    @tag : Int32 = 0

    def initialize(host, port, flags : GlobalFlags)
        socket = TCPSocket.new(host, port)
        context = OpenSSL::SSL::Context::Client.new

        @socket = OpenSSL::SSL::Socket::Client.new(socket, context)
        @flags = flags

        r = next_response
        if !r.is_a?(OKResponse)
            raise BadResponseException.new(r)
        end
    end

    private def put(line : String)
        if @flags.trace_imap
            puts (">" + line)
        end
        @socket << line
        @socket.flush
    end

    private def get : String
        sb = String::Builder.new
        line = "{0}"

        while true
            m = /\{([0-9]+)\}$/.match(line)
            if !m
                break
            end

            slice = Slice(UInt8).new(m[1].to_i)
            @socket.read_fully(slice)
            sb << String.new(slice, "ISO-8859-1")

            line = @socket.gets("\r\n", chomp=true)
            if line.nil?
                raise "Socket unexpectedly closed"
            end
            sb << line
        end

        s = sb.to_s
        if @flags.trace_imap
            puts s
        end
        s
    end

    private def next_response : ImapResponse
        s = get
        begin
            return ImapResponse.parse(s)
        rescue e: UnmatchedException
            puts "#{e} in:\n#{s}\n"
            exit 1
        end
    end

    private def new_tag : String
        @tag = @tag + 1
        "T" + @tag.to_s
    end

    def command(command : String, &block : ImapResponse->Void)
        put("#{new_tag} #{command}\r\n")

        while true
            response = next_response
            break if (response.tag != "*")

            yield response
        end

        if !response.is_a?(OKResponse)
            raise BadResponseException.new(response)
        end
        response
    end

    def login(username : String, password : String, &block : ImapResponse->Void)
        command("login #{quoted(username)} #{quoted(password)}", &block)
    end

    def select(mailbox : String, &block : ImapResponse->Void)
        command("select #{quoted(mailbox)}", &block)
    end
end

private def quoted(s : String)
    '"' + s.gsub(/"/, "\\\"") + '"'
end

class BadResponseException < Exception
    def initialize(r)
        super("bad response from IMAP server: #{r}")
    end
end
