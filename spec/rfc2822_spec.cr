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

    it "headers" do
        testdata = (
            "Simple: some text\r\n" +
            "Needs-Trimming:     some text    \r\n" +
            "Extended: some\r\n" +
            "    more text\r\n" +
            "\teven more text\r\n" +
            "Repeated: real text\r\n" +
            "Repeated: more text\r\n" +
            "\r\n" +
            "Body text\r\n"
        )

        headers = Rfc2822::Headers.new(testdata)
        headers["Missing"]?.should eq nil
        headers["Simple"]?.should eq "some text"
        headers["Needs-Trimming"]?.should eq "some text"
        headers["Extended"]?.should eq "some more text even more text"
        headers["Repeated"]?.should eq "real text"
    end

    it "content-type" do
        t = Rfc2822::ContentType.new(nil)
        t.mime_type.should eq "text/plain"
        t.parameters.should eq Hash(String, String).new

        t = Rfc2822::ContentType.new("text/plain; foo=bar")
        t.mime_type.should eq "text/plain"
        t.parameters.should eq ({"foo" => "bar"})

        t = Rfc2822::ContentType.new("text/plain; foo=bar  ; baz=boo")
        t.mime_type.should eq "text/plain"
        t.parameters.should eq ({"foo" => "bar", "baz" => "boo"})

        t = Rfc2822::ContentType.new("text/plain; foo=\"quoted string\"; bar=mix\"ed \"string")
        t.mime_type.should eq "text/plain"
        t.parameters.should eq ({"foo" => "quoted string", "bar" => "mixed string"})
    end

    it "simple message" do
        testdata = (
            "From: from header\r\n" +
            "\r\n" +
            "body text\r\n"
        )

        message = Rfc2822.parse_message(testdata)
        message.multipart?.should eq false
        message.type.mime_type.should eq "text/plain"
        message.headers["From"].should eq "from header"
        message.bodytext.should eq "body text\r\n"
    end

    it "base64 message" do
        testdata = (
            "Content-Transfer-Encoding: base64\r\n" +
            "\r\n" +
            "SWYgeW91IGNhbiByZWFkIHRoaXMgeW8="
        )

        message = Rfc2822.parse_message(testdata)
        message.multipart?.should eq false
        message.type.mime_type.should eq "text/plain"
        message.bodytext.should eq "If you can read this yo"
        message.subsections.size.should eq 0
    end

    it "multipart message" do
        testdata = (
            "Content-Type: multipart/mixed; boundary=boundary\r\n" +
            "Content-Transfer-Encoding: base64\r\n" +
            "\r\n" +
            "preamble\r\n" +
            "--boundary\r\n" +
            "Content-Type: text/plain\r\n" +
            "\r\n" +
            "subsection 1\r\n" +
            "--boundary\r\n" +
            "Content-Type: text/plain\r\n" +
            "\r\n" +
            "subsection 2\r\n" +
            "--boundary--\r\n" +
            "postamble\r\n"
        )

        message = Rfc2822.parse_message(testdata)
        message.multipart?.should eq true
        message.subsections.size.should eq 2

        ss = message.subsections[0]
        ss.type.mime_type.should eq "text/plain"
        ss.bodytext.should eq "subsection 1"

        ss = message.subsections[1]
        ss.type.mime_type.should eq "text/plain"
        ss.bodytext.should eq "subsection 2"
    end
end
