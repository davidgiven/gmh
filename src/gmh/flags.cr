require "./globals"

module FlagsParser
    def parse(argv : Array(String))
        by_name = {} of String => Flag

        self.all_flags.each do |flag|
            flag.names.each do |name|
                by_name[name] = flag
            end
        end

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

            flag = by_name[arg]?
            if flag.nil?
                raise UserException.new("unrecognised flag '%s'" % arg)
            end
            if flag.parse(arg, opt) && !combined
                i += 1
            end

            i += 1
        end

        self.argv = argv[i..argv.size]
        self
    end
end

macro define_flags
    include FlagsParser

    property argv = [] of String
    getter all_flags = [] of Flag
end

macro define_flags(flags)
    include FlagsParser

    property argv = [] of String

    {% for id, flag in flags %}
        getter {{ id }}_flag = {{ flag }}

        def {{ id }}
            @{{ id }}_flag.value
        end
    {% end %}

    def all_flags : Array(Flag)
        flags = [] of Flag
        {% for id, flag in flags %}
            flags << {{ id }}_flag
        {% end %}
        flags
    end
end

abstract class Flag
    getter names : Array(String)
    getter help : String
    property value : String | Int32 | Bool

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

    def value : String
        @value.as(String)
    end
end

class IntFlag < Flag
    def parse(left, right)
        @value = right.to_i
        true
    end

    def value : Int32
        @value.as(Int32)
    end
end

class BoolFlag < Flag
    def parse(left, right)
        @value = true
        false
    end

    def value : Bool
        @value.as(Bool)
    end
end
