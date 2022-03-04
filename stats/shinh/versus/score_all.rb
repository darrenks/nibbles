hjelly = Hash.new(0)
hgs2 = Hash.new(0)

problems = `ls problems`.split
problems.each{|filename|
   page = File.read("problems/"+filename)
   #p filename
   next if %w"Sierpinski+Fractal.html Halfassed+Encryption+FIXED.html Hello+OCaml.html Reverse+Bits+FIXED.html Signed+Encryption+FIXED.html Signed+Encryption.html".include? filename

   after_lang_ranking = page.split("Language Ranking")[1]
   next if after_lang_ranking == nil
   langs = after_lang_ranking.split("</table>")[0].split("<tr>")


   # <td>48</td><td>Brainfuck</td><td>nuko(cheat)</td><td>1052</td><td>560</td>
   h = {}
   langs[2..-1].map{|lang|
      lang =~ /^<td>\d+<\/td><td>(.+?)<\/td><td>.+?<\/td><td>(\d+)<\/td>/
      h[$1] = $2.to_i
   }

   if h["Jelly"] && h["Nibbles"] #&& h["Jelly"] > 1 && h["Nibbles"] > 1
      hjelly[h["Nibbles"] <=> h["Jelly"]] += 1
   end
   if h["gs2"] && h["Nibbles"] #&& h["gs2"] > 1 && h["Nibbles"] > 1
      hgs2[h["Nibbles"] <=> h["gs2"]] += 1
   end

   #break
}
puts "v jelly (-1 = nibbles wins)"
p hjelly
puts "gs2"
p hgs2