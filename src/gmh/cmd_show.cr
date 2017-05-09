require "./flags"
require "./database"
require "./rfc2822"
require "./html_to_text"
require "io/memory"

class ShowFlags
    define_flags({
        body_type: StringFlag.new(
                ["--body-type"],
                "MIME type of preferred email body.",
                "text/plain"),
    })
end

private def flatten_structure(section : Rfc2822::Section,
        sections = Array(Rfc2822::Section).new) : Array(Rfc2822::Section)
    sections << section
    section.subsections.each do |subsection|
        flatten_structure(subsection, sections)
    end
    sections
end

private def find_body_section(sections : Array(Rfc2822::Section), preferred_type : String) : Rfc2822::Section
    sections.each do |section|
        if section.type.mime_type == preferred_type
            return section
        end
    end

    sections.each do |section|
        if /^text\// =~ section.type.mime_type
            return section
        end
    end

    sections.each do |section|
        if /^multipart\// !~ section.type.mime_type
            return section
        end
    end

    sections[0]
end

def doShowCommand(globalFlags : GlobalFlags)
    flags = ShowFlags.new.parse(globalFlags.argv)

    db = Database.new(globalFlags)
    if flags.argv.size != 1
        raise UserException.new("syntax: show <uid>")
    end

    db.query("SELECT headers, body FROM messages INNER JOIN messageData
              ON messages.gmailId = messageData.docId
              WHERE messages.uid = ?", flags.argv[0].to_i) do |rs|
        headers = rs.read(String)
        headers = headers.gsub(/\s*\r\n\s+/, " ")
        body = rs.read(String)

        main = Rfc2822.parse_message(headers, body)
        puts "Subject: #{main.headers["Subject"]}"
        puts "From: #{main.headers["From"]}"
        puts "Date: #{main.headers["Date"]}"
        puts ""

        parts = flatten_structure(main)
        body_section = find_body_section(parts, flags.body_type)
        charset = body_section.type["charset"]? || "ISO-8859-1"
        body_text = String.new(body_section.body, charset)
        if body_section.type.mime_type == "text/html"
            body_text = html_to_text(body_text)
        end
        puts body_text

        puts ""
        parts.each do |section|
            if !section.multipart?
                s = String.build do |sb|
                    sb << section.type.mime_type
                    name = section.type["name"]?
                    if !name.nil?
                        sb << ": " << name
                    end
                end
                puts s
            end
        end
    end

end
