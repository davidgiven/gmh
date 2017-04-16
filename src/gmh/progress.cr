require "./curses"

module ProgressBarFuncs
    def count(max : Int32)
        pb = ProgressBar.new(max)
        yield pb
        pb.set_progress(max)
        pb.update
    end
end

private def time_in_ms : Int64
    Time.local_ticks / Time::Span::TicksPerMillisecond
end

private def as_time(duration_secs : Float64) : String
    if duration_secs.infinite?
        "(unknown)"
    else
        seconds = (duration_secs % 60).to_i
        minutes = (duration_secs / 60).to_i
        "%dm:%02s" % [minutes, seconds]
    end
end

class ProgressBar
    extend ProgressBarFuncs

    @pb_chars = [" ", "▏","▎","▍","▌","▋","▊","▉","█"]
    @count = 0
    @max : Int32
    @width : Int32
    @start_time : Int64 = time_in_ms
    @last_update_time = 0_i64
    @now = 0_i64

    def initialize(max)
        @max = max
        LibC.setlocale(LibNcursesw::LC_ALL, "")
        LibNcursesw.tgetent(nil, ENV["TERM"])
        @width = LibNcursesw.tgetnum("co").to_i
        LibNcursesw.putp("\n")
        update
    end


    def status_string : String
        per_sec = @count.to_f / (@now - @start_time).to_f * 1000
        time_so_far = (@now - @start_time).to_f / 1000
        estimated_total = @max.to_f / per_sec

        " #{per_sec.to_i}/sec, #{as_time(estimated_total - time_so_far)} left"
    end

    def update
        s = String::Builder.new

        status = status_string
        width = @width - status.size
        if (@count >= @max)
            s << @pb_chars[@pb_chars.size-1] * width
        else
            scaled_progress = @count * width * @pb_chars.size / @max
            num_filled = scaled_progress / @pb_chars.size
            subchar = scaled_progress % @pb_chars.size
            numempty = width - num_filled - 1

            s << @pb_chars[@pb_chars.size-1] * num_filled
            if subchar >= 0
                s << @pb_chars[subchar]
            end
            s << @pb_chars[0] * numempty
        end

        s << status << "\n"
        Termcap.put("up")
        Termcap.put("cd")
        LibNcursesw.putp(s.to_s)

        @last_update_time = @now
    end

    def set_progress(progress : Int32)
        @count = progress
        @now = time_in_ms
        if (@now - @last_update_time) > 200
            update
        end
    end

    def next
        set_progress(@count + 1)
    end
end
