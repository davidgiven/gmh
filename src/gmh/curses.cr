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

module Termcap
    extend self

    @@cache = Hash(String, LibC::Char*).new

    def [](id : String) : LibC::Char*
        s = @@cache[id]?
        if s
            s
        else
            s = LibNcursesw.tgetstr(id, nil)
            @@cache[id] = s
            s
        end
    end

    def put(id : String) : Void
        LibNcursesw.putp(self[id])
    end
end
