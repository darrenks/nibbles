Navbar='<div id="navbar"><ul>
	<li id="nibbles"><a href="index.html">Nibbles</a></li>
	<li><a href="tutorial_basics.html">Tutorial: Basics</a> | <a href="tutorial_ancillary.html">Ancillary</a> | <a href="tutorial_minutiae.html">Minutiae</a></li>
	<li><a href="quickref.html">Quick Ref</a></li>
	<li><a href="https://github.com/darrenks/nibbles">Code: Github</a> | <a href="nibbles-latest.tgz">Download</a></li>
</ul></div>'

allFiles = `cd docs; echo *.md`.split

def convertMd(filename)
	basefile = filename.sub(/\.md$/,'')
	md = File.read("docs/#{filename}")
	md.gsub!('$QuickRef','<a href="quickref.html">Quick Ref</a>')
	md.gsub!('$Feedback','Bug reports, suggestions, and code reviews are appreciated (I\'m new to Haskell), make a push request or email me at <img src="email.png" alt="Image of email" title="" />')
	md.gsub!('$Intro',File.read("README.md").lines.first)
	File.open('t.md','w'){|f|f<<md}
	markdown = `markdown.pl < t.md`
	`rm t.md`
	
	title = markdown[/<h1>(.*?)<\/h1>/,1]
	if title.nil?
		puts 'skipping '+filename
		return
	end
	navbar = if filename=="tutorial_basics.md"
		Navbar.sub('<a href="quickref.html">Quick Ref</a>','<del><a href="quickref.html">Quick Ref</a></del><ins><a href="simpleref.html">Simple Ref</a></ins>')
	else
		Navbar
	end
	File.open('web/site/'+basefile+'.html','w'){|f|f<<'<!DOCTYPE HTML>
<html><!-- This file is automatically generated from generate_site.rb by reading '+filename+' --><head><title>'+title+'</title><link rel="stylesheet" href="style.css"></head><body>'+navbar+'<div id="content">'+markdown+'</div></body></html>'}
end

allFiles.each{|f|
	convertMd(f)
}