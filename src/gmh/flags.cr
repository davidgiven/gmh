class Flagset
    @by_name = {} of String => Flag
    @by_id = {} of String => Flag

    def []=(id : String, flag : Flag)
        @by_id[id] = flag
        flag.names.each do |name|
            @by_name[name] = flag
        end
    end

    def parse(argv : Array(String)) : ParsedFlags
        i = 0
        while i < argv.size
            arg = argv[i]
            opt = argv[i+1]?
            if opt.nil?
                opt = ""
            end

            combined = false
            if /^--\w+$/.match(arg)
                # --foo arg
            elsif match = /^(--\w+)=(.*)$/.match(arg)
                # --foo=arg
                arg = match[1]
                opt = match[2]
                combined = true
            elsif /^-\w$/.match(arg)
                # -f arg
            elsif match = /^(-\w)(.*)$/.match(arg)
                # -farg
                arg = match[1]
                opt = match[2]
                combined = true
            else
                # not an arg
                break
            end

            flag = @by_name[arg]
            if flag.parse(arg, opt) && !combined
                i += 1
            end

            i += 1
        end

        ParsedFlags.new(@by_id, argv[i..argv.size])
    end
end

class ParsedFlags
    getter flags : Hash(String, Flag)
    getter rest : Array(String)

    def initialize(flags, rest)
        @flags = flags
        @rest = rest
    end

    def [](id)
        @flags[id].value
    end
end

abstract class Flag
    getter names : Array(String)
    getter help : String
    getter value : String | Int32

    def initialize(names, help, default)
        @names = names
        @help = help
        @value = default
    end

    abstract def parse(left : String, right : String) : Bool
end

class StringFlag < Flag
    def parse(left, right)
        @value = right
        true
    end
end

class IntFlag < Flag
    def parse(left, right)
        @value = right.to_i
    end
end
