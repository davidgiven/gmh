require "spec"
require "../src/gmh/rfc2822"

describe "rfc2822" do
    it "unicode" do
        Rfc2822.parse_encoded_word(
            "plain text").should eq "plain text"
        Rfc2822.parse_encoded_word(
            "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=").should eq "If you can read this yo"
        Rfc2822.parse_encoded_word(
            "=?iso-8859-1?q?plain text?=").should eq "plain text"
        Rfc2822.parse_encoded_word(
            "=?ISO-8859-1?Q?Patrik_F=E4ltstr=F6m?=").should eq "Patrik_Fältström"

        Rfc2822.parse_encoded(
            "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?= foo =?iso-8859-1?q?plain text?="
            ).should eq "If you can read this yo foo plain text"
    end
end
