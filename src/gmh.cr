require "./gmh/imap"

puts "connecting"
imap = Imap.new("imap.gmail.com", 993)
imap.login("david.given@gmail.com", "")
imap.select("[Gmail]]/All Mail")
puts "done"
