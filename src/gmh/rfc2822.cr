require "string_scanner"

module Rfc2822
    extend self

    class CannotParseTimeException < Exception
    end

    @@months = {
        "Jan" => 1,
        "Feb" => 2,
        "Mar" => 3,
        "Apr" => 4,
        "May" => 5,
        "Jun" => 6,
        "Jul" => 7,
        "Aug" => 8,
        "Sep" => 9,
        "Oct" => 10,
        "Nov" => 11,
        "Dec" => 12
    }

    @@timezones = {
        "" => 0,
        "Z" => 0,
        "UT" => 0,
        "UTC" => 0,
        "GMT" => 0,
        "BST" => 1,
        "EST" => -5,
        "EDT" => -4,
        "CST" => -6,
        "CDT" => -5,
        "MST" => -7,
        "MDT" => -6,
        "PST" => -8,
        "PDT" => -7
    }

    def parse_time(s : String) : Time
        year = 0
        month = 0
        day = 0
        hour = 0
        minute = 0
        second = 0
        tz_minutes = 0

        ss = StringScanner.new(s)
        ss.scan(/\s*(Mon|Tue|Wed|Thu|Fri|Sat|Sun)?,?\s+/)

        if ss.scan(/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+/)
            month = @@months[ss[1]]

            ss.scan(/([0-9]*)\s+/)
            day = ss[1].to_i
        elsif ss.scan(/([0-9]*)\s+/)
            day = ss[1].to_i

            ss.scan(/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+/)
            month = @@months[ss[1]]

            ss.scan(/([0-9]*)\s*/)
            year = ss[1].to_i
        end

        ss.scan(/([0-9]*):/)
        hour = ss[1].to_i

        if ss.scan(/([0-9]*):/)
            minute = ss[1].to_i

            ss.scan(/([0-9]*)\s*/)
            second = ss[1].to_i
        elsif ss.scan(/([0-9]*)\s*/)
            # no second field
            minute = ss[1].to_i
            second = 0
        else
            raise CannotParseTimeException.new("~Invalid time '#{s}'")
        end

        if year == 0
            ss.scan(/([0-9]*)\s+/)
            year = ss[1].to_i
        end

        ss.scan(/([^\s]*)/)
        if @@timezones[ss[1]]?
            tz_minutes = @@timezones[ss[1]] * 60
        else
            m = /^([+-]?)(\d\d)(\d\d)$/.match(ss[1])
            if !m
                m = /^([+-]?)(\d)(\d\d)$/.match(ss[1])
            end
            if !m
                raise CannotParseTimeException.new("~Invalid time '#{s}'")
            end
            tz_minutes = m[2].to_i * 60 + m[3].to_i
            if m[1] == "-"
                tz_minutes = -tz_minutes
            end
        end

        if year < 100
            # ugggh
            year += 1900
        end

        tz = Time::Span.new(tz_minutes.to_i64 * Time::Span::TicksPerMinute)
        (Time.new(year, month, day, hour, minute, second, 0, Time::Kind::Utc) - tz)
    end
end
