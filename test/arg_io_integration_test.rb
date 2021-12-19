# test nibbles with some features that would do more complicated things otherwise
test_program = '
p$ "\\n"
p@ "\\n"
p_ "\\n"

'
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc -O -package ghc -Wno-tabs nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

arg2 = "([-1,2],[(1,'a',\"bcd\")])".gsub(/[()\]\[\'\"]/){"\\"+$&}
#puts arg2
out = `./nibbles #{testnbl} 12 #{arg2} 2> /dev/null`
out == "12\n[-1,2]\n[(1,'a',\"bcd\")]\n" or raise 'generated haskell produces wrong output %p' % out

puts 'pass arg io integration test'
