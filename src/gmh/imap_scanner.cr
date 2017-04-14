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
