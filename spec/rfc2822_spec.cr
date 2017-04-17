require "spec"
require "../src/gmh/rfc2822"

describe "rfc2822" do
    it "time parsing" do
        Rfc2822.parse_time("Sun Jan 18 22:06:34 1998 MST").to_s.should eq "1998-01-19 05:06:34 UTC"
        Rfc2822.parse_time("Sun Jan 18 22:06:34 1998 BST").to_s.should eq "1998-01-18 21:06:34 UTC"
        Rfc2822.parse_time("Sun Jan 18 22:06:34 1998 -0800").to_s.should eq "1998-01-19 06:06:34 UTC"
        Rfc2822.parse_time("Wed, 8 Dec 1999 10:55:47 +0000").to_s.should eq "1999-12-08 10:55:47 UTC"
        Rfc2822.parse_time("13 Apr 94 10:55:47").to_s.should eq "1994-04-13 10:55:47 UTC"
        Rfc2822.parse_time("Fri, 10 Mar 2000 02:36 +0000 (GMT Standard Time)").to_s.should eq "2000-03-10 02:36:00 UTC"
        Rfc2822.parse_time("Fri, 30 Aug 2002 08:30:55  0000").to_s.should eq "2002-08-30 08:30:55 UTC"
        Rfc2822.parse_time("Sat, 27 Jul 2002 14:08:19 -800").to_s.should eq "2002-07-27 22:08:19 UTC"
    end
end
