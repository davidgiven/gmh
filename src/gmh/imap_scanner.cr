alias ImapElement = String | Array(ImapElement)

class UnmatchedException < Exception
    def initialize(s)
        super("cannot match '#{s}...'")
    end
end

class ImapResponseScanner < StringScanner

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

    def expect_eol : Void
        if !eos?
            raise UnmatchedException.new("[end of line]")
        end
    end

    def expect_tag : String
        tag = expect(/\*|[A-Za-z0-9]+/)
        expect_ws
        tag.as(String)
    end

    def expect_atom : String
        atom = expect(/[^(){ %"[\]]+/)
        expect_ws
        atom.as(String)
    end

    def at_atom(want) : Bool
        begin
            expect_atom(want)
            true
        rescue UnmatchedException
            false
        end
    end

    def expect_atom(want) : Void
        got = expect_atom
        if (got != want)
            raise UnmatchedException.new(got)
        end
    end

    def expect_string : String
        if scan(/"/)
            s = String::Builder.new
            while true
                if c = scan(/\\(["\\])/)
                    s << c[1]
                elsif scan(/"/)
                    break
                else
                    s << scan(/./)
                end
            end
            expect_ws

            s.to_s
        else
            raise UnmatchedException.new(peek(20))
        end
    end

    def expect_raw_string : String
        self.expect(/\{([0-9]+)}/)
        bytes = self[1].to_i
        s = self.peek(bytes)
        self.offset = self.offset + bytes
        s
    end

    def expect_trailing : String
        scan(/.*/).as(String)
    end

    def expect_element : ImapElement
        case self.peek(1)
            when "("
                list = [] of ImapElement
                expect_open_paren
                while !at_close_paren
                    list << expect_element
                end
                expect_close_paren
                list

            when "\""
                expect_string

            when "{"
                expect_raw_string

            else
                expect_atom
        end
    end

    def expect_bracketed_list : Array(ImapElement)
        list = [] of ImapElement
        expect_open_sq_bracket
        while !at_close_sq_bracket
            list << expect_element
        end
        expect_close_sq_bracket
        list
    end

    def at_open_paren : Bool
        return self.peek(1) == "("
    end

    def expect_open_paren : Void
        expect(/\(/)
        expect_ws
    end

    def at_close_paren : Bool
        return self.peek(1) == ")"
    end

    def expect_close_paren : Void
        expect(/\)/)
        expect_ws
    end

    def at_open_sq_bracket : Bool
        return self.peek(1) == "["
    end

    def expect_open_sq_bracket : Void
        expect(/\[/)
        expect_ws
    end

    def at_close_sq_bracket : Bool
        return self.peek(1) == "]"
    end

    def expect_close_sq_bracket : Void
        expect(/\]/)
        expect_ws
    end

    def expect_nz_int : Int32
        s = expect(/[0-9]+/)
        expect_ws
        s.to_i
    end
end
