require 'json'

Atoms = Hash.new(0)

`ls problems`.split.each{|f|
   next if f == "0_5+broken+keyboard.html" || f == "b+ab.html"
   s=File.read('./problems/'+f)
   p f
   j=JSON.parse(s)


   def ptree(h,depth=0)
      h.each{|k,v|
         next if k=="childs"
#          puts " "*depth + k + " -> " + v.to_s
      }
      if h["childs"]
         h["childs"].each{|child|
            ptree(child,depth+1)
#             puts " "*depth + ","
         }
      else # atom
         # remove arg desc
         desc = h["desc"].split("= ")[0]

         Atoms[(h["lit"]||"") + " " + desc] += 1

         # cound input args too
         desc2 = h["desc"].split("= ")[1]
         Atoms[desc2] += 1 if desc2&&desc2.size > 1

      end
   end

   ptree j["code"]
}

Atoms.sort_by{|k,v|v}.each{|k,v|
   puts k + " -> " + v.to_s
}