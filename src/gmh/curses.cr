@[Link("ncursesw")]

lib LibC
  fun setlocale(cat : Int32, locale : LibC::Char*) : LibC::Char*
end

lib LibNcursesw
  # setlocale
  LC_ALL = 6

  fun tgetent(bp : LibC::Char*, name : LibC::Char*) : Int32
  fun tgetflag(id : LibC::Char*) : Int32
  fun tgetnum(id : LibC::Char*) : Int32
  fun tgetstr(id : LibC::Char*, area : LibC::Char**) : LibC::Char*
  fun putp(str : LibC::Char*) : Int32
end

LibC.setlocale(LibNcursesw::LC_ALL, "")
LibNcursesw.tgetent(nil, ENV["TERM"])

module Termcap
    extend self

    @@cache = Hash(String, String).new
    @@width : Int32 = LibNcursesw.tgetnum("co").to_i

    def [](id : String) : String
        s = @@cache[id]?
        if s
            s
        else
            s = String.new(LibNcursesw.tgetstr(id, nil))
            @@cache[id] = s
            s
        end
    end

    def put(id : String) : Void
        LibNcursesw.putp(self[id])
    end

    def width : Int32
        @@width
    end
end
