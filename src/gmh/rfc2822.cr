require "string_scanner"
require "base64"

module Rfc2822
    extend self

    def parse_encoded_word(s : String)
        m = /^=\?([^?]+)\?([^?]+)\?([^?]*)\?=/.match(s)
        if m
            charset = m[1]
            encoding = m[2]
            sourcetext = m[3]

            case encoding
                when "Q", "q"
                    t = sourcetext.gsub(/=([a-fA-F0-9][a-fA-F0-9])/) do |_, e|
                        e[1].to_i32(16).chr
                    end
                    desttext = t.encode("ISO-8859-1")
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
end
