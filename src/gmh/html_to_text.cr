require "myhtml"

private def walk_children(sb, node) : Void
    node.children.each do |child|
        walk(sb, child)
    end
end

private def walk(sb, node) : Void
    case node.tag_name
        when "head", "style", "script"
            return

        when "p", "div", "tr"
            sb << "\n"
            walk_children(sb, node)
            sb << "\n"

        when "td"
            sb << " "
            walk_children(sb, node)
            sb << " "

        when "br"
            sb << "\n"

        when "li"
            sb << "\n* "
            walk_children(sb, node)
            sb << "\n"

        when "-text"
            sb << node.tag_text

        else
            walk_children(sb, node)
    end
end

def html_to_text(input : String) : String
    parser = Myhtml::Parser.new(input)
    sb = String::Builder.new
    walk(sb, parser.root!)
    text = sb.to_s
    return text.gsub(/ +/, " ").gsub(/\n +/, "\n").gsub(/\n+/, "\n")
end
