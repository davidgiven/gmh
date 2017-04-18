require "spec"
require "../src/gmh/imap_responses"

describe "ImapResponseScanner" do
    it "elements" do
        ImapResponseScanner.new("ATOM").expect_element.should eq "ATOM"
        ImapResponseScanner.new("(ONE TWO THREE)").expect_element.should eq ["ONE", "TWO", "THREE"]
    end

    it "strings" do
        ImapResponseScanner.new("string").expect_element.should eq "string"
        ImapResponseScanner.new("\"string\"").expect_element.should eq "string"
        ImapResponseScanner.new("\"st\\ring\"").expect_element.should eq "st\\ring"
        ImapResponseScanner.new("\"st\\\\ring\"").expect_element.should eq "st\\ring"
        ImapResponseScanner.new("\"st\\\"ring\"").expect_element.should eq "st\"ring"
        ImapResponseScanner.new("{3}foobar").expect_element.should eq "foo"

        ImapResponseScanner.new("\\string").expect_element.should eq "\\string"
        ImapResponseScanner.new("$string").expect_element.should eq "$string"
        ImapResponseScanner.new("str-ing").expect_element.should eq "str-ing"

        s = ImapResponseScanner.new("\"string\" ")
        s.expect_element
        s.expect_eol

        s = ImapResponseScanner.new("{3}foobar")
        s.expect_element.should eq "foo"
        s.expect_element.should eq "bar"
        s.expect_eol

        s = ImapResponseScanner.new("{3}foo bar")
        s.expect_element.should eq "foo"
        s.expect_element.should eq "bar"
        s.expect_eol
    end
end

describe "ImapResponseParser" do
    it "permanentflags" do
        s = %{[PERMANENTFLAGS (\Answered \Flagged \Draft \Deleted \Seen $Forwarded $MDNSent $NotPhishing $Phishing $label1 $label2 $label3 Junk NonJunk Old receipt-handled \*)] Flags permitted.}
        r = PermanentFlagsOKResponse.new("*", s, ImapResponseScanner.new(s))
    end

    it "fetches" do
        s = %{FETCH (X-GM-MSGID 1327597056069823273 X-GM-LABELS ("\\Inbox" "\\Sent") UID 26149 MODSEQ (4187954) FLAGS (\Seen))}
        r = FetchResponse.new("*", s, ImapResponseScanner.new(s))

        s = %{FETCH (BODY[] {3}foo)}
        r = FetchResponse.new("*", s, ImapResponseScanner.new(s))
    end
end

describe "ImapEnvelope" do
    it "parsing" do
        s = %{("Wed, 07 Nov 2012 18:46:46 +0000" \
                "subject" \
                (("From" NIL "from" "example.com")) \
                (("Sender" NIL "sender" "example.com")) \
                (("ReplyTo1" NIL "1" "example.com")("ReplyTo2" NIL "2" "example.com")) \
                ((NIL NIL "to" "example.com")) \
                NIL \
                NIL \
                NIL \
                "<20cf300e524bfb64cb04cdec2372@google.com>")}
        e = ImapResponseScanner.new(s).expect_element
        env = ImapEnvelope.new(e.as(Array(ImapElement)))
        env.subject.should eq "subject"
        env.sender.not_nil!.to_a.should eq [ImapEmail.new("Sender", "sender@example.com")]
        env.from.not_nil!.to_a.should eq [ImapEmail.new("From", "from@example.com")]
        env.reply_to.not_nil!.to_a.should eq [
            ImapEmail.new("ReplyTo1", "1@example.com"), ImapEmail.new("ReplyTo2", "2@example.com")]
        env.to.not_nil!.to_a.should eq [ImapEmail.new("to@example.com", "to@example.com")]
        env.cc.should eq Set(ImapEmail).new
        env.bcc.should eq Set(ImapEmail).new
        env.in_reply_to.should eq nil
        env.message_id.should eq "<20cf300e524bfb64cb04cdec2372@google.com>"
    end
end
