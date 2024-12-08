require 'json'

Ops = 5.times.map{|arity| {} }

def simplify_tree(h)
   desc = h["desc"].split("= ")[0] # remove arg description
   name = (h["lit"]||"") + " " + desc
   children = (h["childs"]||[]).map{|child|
      simplify_tree(child)
   }

   # ops that take an auto value arg are really a different op of 1 less arity
   children.each.with_index{|child,index| if child[1] == "auto"
      name += " auto"+(index+1).to_s
   end}

   children.filter!{|child|!%w"null auto".include?(child[1])}

   # fns are annotated with "args" by the commenter
   type =  h["args"] ? "fn" : h["type"]

   types = children.map{|c|c[1]}
   children.each.with_index{|a,index|
      child_name,child_type,_=a
      combo_name = "("+name +",a"+(index+1).to_s+" "+ child_name+")"
      (Ops[children.size][types.sort]||={})[combo_name]||=0
      Ops[children.size][types.sort][combo_name]+=1
   }

   [name, type, children]
end

`ls problems`.split.each{|f|
  next if f == "0_5+broken+keyboard.html" || f == "b+ab.html" || f == "nightmare+problem.html"
   s=File.read('./problems/'+f)
   j=JSON.parse(s)

   simplify_tree j["code"]
}

Ops.each.with_index{|ops,arity|
   puts "arity %d"%arity
   ops.sort_by{|types,ops2|ops2.map{|op,count|count}.sum}.each{|a|
      types,ops2=a
      sum = ops2.map{|c,s|s}.sum
      puts "types %s, sum = %d"%[types*", ",sum]
      ops2.sort_by{|op,count|count}.each{|a|
         op,count=a
         puts "%s -> %d" % [op,count] if count > 1
      }
      puts
   }
   puts
}