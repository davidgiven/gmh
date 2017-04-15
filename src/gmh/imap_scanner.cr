alias ImapElement = String | Array(ImapElement)

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
        atom = expect(/[A-Za-z0-9=\\$*-]+/)
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

    def expect_element : ImapElement
        if at_open_paren
            list = [] of ImapElement
            expect_open_paren
            while !at_close_paren
                list << expect_element
            end
            expect_close_paren
            list
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
