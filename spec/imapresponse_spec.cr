require "spec"
require "../src/gmh/imap_responses"

describe "ImapResponseScanner" do
    describe "elements" do
        ImapResponseScanner.new("ATOM").expect_element.should eq "ATOM"
        ImapResponseScanner.new("(ONE TWO THREE)").expect_element.should eq ["ONE", "TWO", "THREE"]
    end

    describe "strings" do
        ImapResponseScanner.new("string").expect_element.should eq "string"
        ImapResponseScanner.new("\"string\"").expect_element.should eq "string"
        ImapResponseScanner.new("\"st\\ ring\"").expect_element.should eq "st ring"
        ImapResponseScanner.new("\"st\\\"ring\"").expect_element.should eq "st\"ring"

        ImapResponseScanner.new("\\string").expect_element.should eq "\\string"
        ImapResponseScanner.new("$string").expect_element.should eq "$string"
        ImapResponseScanner.new("str-ing").expect_element.should eq "str-ing"

        s = ImapResponseScanner.new("\"string\" ")
        s.expect_element
        s.expect_eol
    end
end

describe "ImapResponseParser" do
    describe "permanentflags" do
        s = %{[PERMANENTFLAGS (\Answered \Flagged \Draft \Deleted \Seen $Forwarded $MDNSent $NotPhishing $Phishing $label1 $label2 $label3 Junk NonJunk Old receipt-handled \*)] Flags permitted.}
        r = PermanentFlagsOKResponse.new("*", s, ImapResponseScanner.new(s))
    end

    describe "fetches" do
        s = %{FETCH (X-GM-MSGID 1327597056069823273 X-GM-LABELS ("\\Inbox" "\\Sent") UID 26149 MODSEQ (4187954) FLAGS (\Seen))}
        r = FetchResponse.new("*", s, ImapResponseScanner.new(s))
    end
end
