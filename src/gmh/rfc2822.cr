require "string_scanner"
require "base64"

module Rfc2822
    extend self

    def decode_8bits(body : String) : Bytes
        return body.encode("ISO-8859-1")
    end

    def decode_quoted_printable(body : String) : Bytes
        t = body.gsub(/=([a-fA-F0-9][a-fA-F0-9])/) do |_, e|
            e[1].to_i32(16).chr
        end
        return t.encode("ISO-8859-1")
    end

    def parse_encoded_word(s : String)
        m = /^=\?([^?]+)\?([^?]+)\?([^?]*)\?=/.match(s)
        if m
            charset = m[1]
            encoding = m[2]
            sourcetext = m[3]

            case encoding
                when "Q", "q"
                    desttext = decode_quoted_printable(sourcetext)
                when "B", "b"
                    desttext = Base64.decode(sourcetext)
                else
                    return s
            end

            return String.new(desttext, charset)
        else
            return s
        end
    end

    def parse_encoded(s : String)
        return s.gsub(/=\?[^?]+\?[^?]+\?[^?]*\?=/) do |m|
            parse_encoded_word(m)
        end
    end

    class Headers
        @headers = Hash(String, String).new

        def initialize(headers : String)
            ss = StringScanner.new(headers + "\r\n")

            header = ""
            while true
                if ss.scan(/$/)
                    break
                elsif ss.scan(/\r\n/)
                    break
                elsif ss.scan(/([\x21-\x7e]+):\s*(.*)\r\n/)
                    header = ss[1]
                    value = ss[2]
                    if !@headers[header]?
                        @headers[header] = value.strip
                    end
                elsif ss.scan(/[ \t]+(.*) *\r\n/)
                    @headers[header] += " " + ss[1]
                else
                    raise "invalid header value at '#{ss.peek(20)}...'"
                end
            end
        end

        def [](key : String) : String
            @headers[key]
        end

        def []?(key : String) : String?
            @headers[key]?
        end
    end

    class ContentType
        getter mime_type : String
        getter parameters = Hash(String, String).new

        def initialize(content_type : String?)
            if content_type.nil?
                content_type = "text/plain"
            end

            ss = StringScanner.new(content_type)
            ss.scan(/([^; ]+)\s*/)
            @mime_type = ss[1]
            while ss.scan(/;\s*/)
                ss.scan(/([^=]+)=/)
                key = ss[1]
                value = String::Builder.new

                while true
                    if ss.eos? || (ss.peek(1) == ";")
                        break
                    elsif ss.scan(/"/)
                        while true
                            if ss.eos?
                                break
                            elsif ss.scan(/([^\\"]+)/)
                                value << ss[1]
                            elsif ss.scan(/\\(.)/)
                                value << ss[1]
                            elsif ss.scan(/"/)
                                break
                            end
                        end
                    else
                        ss.scan(/([^";]*)/)
                        value << ss[1]
                    end
                end
                parameters[key] = value.to_s.strip
            end
        end

        def multipart? : Bool
            @mime_type.starts_with?("multipart/")
        end

        def [](id : String) : String
            @parameters[id]
        end

        def []?(id : String) : String?
            @parameters[id]?
        end
    end

    def parse_message(header_text : String, message_text : String) : Section
        headers = Headers.new(header_text)
        content_type = ContentType.new(headers["Content-Type"]? || "text/plain")

        if content_type.multipart?
            boundary = content_type["boundary"] || ""
            message_text = message_text.sub("\r\n--" + boundary + "--\r\n", "\r\n--" + boundary + "\r\n")
            message_text = "\r\n" + message_text
            subsections_text = message_text.split("\r\n--" + boundary + "\r\n")
            subsections = Array(Section).new
            subsections_text[1..subsections_text.size-2].each do |text|
                subsections << parse_message(text)
            end
            Section.new(headers, subsections, content_type)
        else
            case (headers["Content-Transfer-Encoding"]? || "7bit")
                when "base64"
                    bytes = Base64.decode(message_text)
                else
                    bytes = message_text.encode("ISO-8859-1")
            end
            Section.new(headers, bytes, content_type)
        end
    end

    def parse_message(message : String) : Section
        m = /(^|.*?\r\n)\r\n(.*)$/m.match(message).not_nil!
        parse_message(m[1], m[2])
    end

    class Section
        getter headers : Headers
        getter body : Bytes
        getter subsections : Array(Section)
        getter type : ContentType

        def initialize(headers : Headers, body : Bytes, content_type : ContentType)
            @headers = headers
            @body = body
            @subsections = Array(Section).new
            @type = content_type
        end

        def initialize(headers : Headers, subsections : Array(Section), content_type : ContentType)
            @headers = headers
            @body = "".encode("ISO-8859-1")
            @subsections = subsections
            @type = content_type
        end

        def multipart? : Bool
            @type.multipart?
        end

        def bodytext : String
            String.new(@body.not_nil!, "UTF-8")
        end
    end
end
