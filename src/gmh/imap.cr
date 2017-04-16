require "socket"
require "openssl"
require "string_scanner"
require "./imap_responses"

class Imap
    @socket : OpenSSL::SSL::Socket
    @flags : GlobalFlags
    @response_handler : (ImapResponse)->
    @tag : Int32 = 0

    def initialize(host, port, flags : GlobalFlags, &response_handler : ImapResponse->Void)
        socket = TCPSocket.new(host, port)
        context = OpenSSL::SSL::Context::Client.new

        @socket = OpenSSL::SSL::Socket::Client.new(socket, context)
        @flags = flags
        @response_handler = response_handler

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
    end

    private def get : String
        line = @socket.gets("\r\n", chomp=true)
        if line.nil?
            raise "Socket unexpectedly closed"
        end
        if @flags.trace_imap
            puts ("<" + line)
        end
        line
    end

    private def next_response : ImapResponse
        ImapResponse.parse(get)
    end

    private def wait_for_response : ImapResponse
        while true
            response = next_response
            break if (response.tag != "*")

            @response_handler.call(response)
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

    def command(command : String)
        put("#{new_tag} #{command}\r\n")
        wait_for_simple_command_response
    end

    def login(username : String, password : String)
        command("login #{quoted(username)} #{quoted(password)}")
    end

    def select(mailbox : String)
        command("select #{quoted(mailbox)}")
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
