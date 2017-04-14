require "spec"
require "../src/gmh/flags"

class TestFlags
    define_flags ({
        string: StringFlag.new(["-s", "--string"], "description", ""),
        int: IntFlag.new(["-i"], "", 0),
        bool: BoolFlag.new(["-b"], "", true)
    })
end

describe "Flags" do
    describe "basic parsing" do
        flags = TestFlags.new

        describe "flag parameters" do
            flags.parse(["-sfoo"]).string.should eq "foo"
            flags.parse(["-s", "foo"]).string.should eq "foo"
            flags.parse(["--string=foo"]).string.should eq "foo"
            flags.parse(["--string", "foo"]).string.should eq "foo"
        end

        describe "rest" do
            flags.parse(["-sfoo"]).argv.should eq [] of String
            flags.parse(["-sfoo", "baz"]).argv.should eq ["baz"]
            flags.parse(["baz", "-sfoo"]).argv.should eq ["baz", "-sfoo"]
        end

        describe "unrecognized" do
            begin
                flags.parse(["-z"])
                fail "didn't throw"
            rescue UserException
            end
        end
    end

    describe "flag types" do
        flags = TestFlags.new

        flags.parse(["-s42"]).string.should eq "42"
        flags.parse(["-i42"]).int.should eq 42
        flags.parse(["-b"]).bool.should eq true
    end
end
