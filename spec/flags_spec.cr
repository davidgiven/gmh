require "spec"
require "../src/gmh/flags"

describe "Flags" do
    describe "basic parsing" do
        flagset = Flagset{
            "flag" => StringFlag.new(["-s", "--string"], "description", "")
        }

        describe "flag parameters" do
            flagset.parse(["-sfoo"])["flag"].should eq "foo"
            flagset.parse(["-s", "foo"])["flag"].should eq "foo"
            flagset.parse(["--string=foo"])["flag"].should eq "foo"
            flagset.parse(["--string", "foo"])["flag"].should eq "foo"
        end

        describe "rest" do
            flagset.parse(["-sfoo"]).rest.should eq [] of String
            flagset.parse(["-sfoo", "baz"]).rest.should eq ["baz"]
            flagset.parse(["baz", "-sfoo"]).rest.should eq ["baz", "-sfoo"]
        end

        describe "unrecognized" do
            begin
                flagset.parse(["-z"])
                fail "didn't throw"
            rescue UserException
            end
        end
    end

    describe "flag types" do
        flagset = Flagset{
            "string" => StringFlag.new(["-s"], "", ""),
            "int" => IntFlag.new(["-i"], "", 0)
        }

        flagset.parse(["-s42"])["string"].should eq "42"
        flagset.parse(["-i42"])["int"].should eq 42
    end
end
