Navbar='<div id="navbar"><ul>
	<li id="nibbles"><a href="index.html">Nibbles</a></li>
	<li><a href="tutorial_basics.html">Basics</a> | <a href="tutorial_ancillary.html">Ancillary</a> | <a href="tutorial_tour.html">Tour</a> | <a href="tutorial_minutiae.html">Minutiae</a></li>
	<li><a href="quickref.html">Quick Ref</a></li>
	<li><a href="install.html">Install</a> | <a href="https://github.com/darrenks/nibbles">Source</a></li>
</ul></div>'

def addTest(raw, prog, input, output)
	File.open('test/TutorialTests.hs','a'){|f|f.puts "-- #{raw}Test %s : %s -> %s" % [input, prog, output]}
end

def getProg(pre)
	pre.reverse[/\A.*?\n\n/m].reverse.gsub(/\#.*/,'').gsub("\n", ' ').gsub(/\s+/,' ').split(/\$(Hidden)?Output/)[0]
end

def removeLeadingTabs(s)
	s.gsub(/^\t/,'')
end

def convertBT(s)
	s.gsub('BT','`')
end

def show s
	s.inspect.gsub('\\#','#')
end

def convertTests(md)
	md.gsub!(/\$(Hidden)?Output ?(".*?")?\n((\t.*?\n)+)/m) {
		addTest("Raw", getProg($`), $2 || "", show(removeLeadingTabs($3)))
		if $1 # Hidden
			''
		else
			"$Gives\n\n"+$3++"\n"
		end
	}
	md.gsub!(/`([^`]+)` ?-> ?`(.*?)`/) {
		addTest("", convertBT($1), "", $2)
		"`" + convertBT($1) + "`" + " &#x2907; " + "`" + $2 + "`"
	}
end

allFiles = `cd docs; echo *.md`.split
`echo > test/TutorialTests.hs`

def convertMd(filename)
	basefile = filename.sub(/\.md$/,'')
	md = File.read("docs/#{filename}")
	md.gsub!('$QuickRef','[Quick Ref](quickref.html)')
	md.gsub!('$Feedback','Bug reports, pain points, suggestions, and code reviews are appreciated (I\'m new to Haskell), make a push request or email me at ![image of email](email.png)')
	md.gsub!('$Intro',File.read("README.md").lines.first)
	convertTests(md)
	File.open('t.md','w'){|f|f<<md}
	markdown = `markdown.pl < t.md`
	`rm t.md`
	markdown.gsub!(/\<p\>\$Solution\<\/p\>(.*?)\<p\>\$EndSolution\<\/p\>/m,
		'<details><summary>Solution</summary>\\1<hr></details>')
	markdown.gsub!("<p>$Gives</p>\n\n<pre><code>",'<pre><code class="result">')

	# pre shows up too small on mobile...
	markdown.gsub!("<pre>",'<div class="prog">')
	markdown.gsub!("</pre>",'</div>')
	
	title = markdown[/<h1>(.*?)<\/h1>/,1]
	
	markdown.gsub!(/<(h[1-3])>(.*?)<\/\1>/){"<#$1 id=\"#{$2.downcase.tr'^a-z0-9',''}\">#$2</#$1>"}
	
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
<html><!-- This file is automatically generated from generate_site.rb by reading '+filename+' --><head><title>'+title+'</title><link rel="stylesheet" href="style.css"><link rel="icon" type="image/x-icon" href="favicon.ico" /></head><body>'+navbar+'<div id="content">'+markdown+'</div></body></html>'}
end

allFiles.each{|f|
	convertMd(f)
}