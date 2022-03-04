require 'open-uri'
w=File.read('all.html')

w.scan(/href=\"(\/p\.rb\?(.+))\"/).each{|url,name|
   p [url,name]
	f = "problems/#{name}.html"
	next if File.exist?(f)
	puts "http://golf.shinh.org"+url
	page = URI.open("http://golf.shinh.org"+url).read
	File.open(f,"w") {|file| file << page }
   sleep 1
}
