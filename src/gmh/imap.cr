require "socket"
require "openssl"
require "string_scanner"
require "./imap_responses"

class Imap
    @socket : OpenSSL::SSL::Socket
    @tag : Int32 = 0
    @capabilities = Set(String).new
    @response_handler : (Response)->

    def initialize(host, port, response_handler)
        socket = TCPSocket.new(host, port)
        context = OpenSSL::SSL::Context::Client.new

        @socket = OpenSSL::SSL::Socket::Client.new(socket, context)
        @response_handler = response_handler

        r = next_response
        if !r.is_a?(OKResponse)
            raise BadResponseException.new(r)
        end
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
        Response.parse(get)
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
