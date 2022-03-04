require 'open-uri'

# t="http://www.tailsteam.com/cgi-bin/nbbdag/commenter.pl?stable+partition/darrenks_1640230125&format=json&codever=020"

# s="<h2><a href="http://golf.shinh.org/p.rb?stable+partition">stable partition</a></h2><table border="1"><tr><th>Rank</th><th>User</th><th>Size</th><th>Time</th><th>Date</th><th>Statistics</th></tr><tr><td>1</td><td><a href="commenter.pl?stable+partition/darrenks_1640230125&nbb">darrenks</a></td><td>5</td><"

s=File.read("index.pl").split("golf.shinh.org")[2..-3]
s.map{|prob|
   dats = prob.scan(/(commenter.pl\?(.*?)\/.*?\&)nbb\">.*?<\/a><\/td><td>(.*?)<\/td>/)
   prob=nil
   link = dats.map.with_index{|dat,index|
      link,probx,size=dat
      prob=probx
      [[size.to_f,index],link]
   }.sort_by{|size,link|size}.map{|size,link|link}[0]
   puts prob

   f = "problems/#{prob}.html"
	next if File.exist?(f)
   url = "http://www.tailsteam.com/cgi-bin/nbbdag/"+link+"format=json&codever=024"
   puts url
	page = URI.open(url).read
	File.open(f,"w") {|file| file << page }
   sleep 1
}


#commenter.pl?stable+partition/darrenks_1640230125&nbb
#commenter.pl?stable+partition/darrenks_1640230125&format=json&codever=020


