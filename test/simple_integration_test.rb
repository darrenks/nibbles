# test nibbles with some features that would do more complicated things otherwise
test_program = "+$ 2 ,,6"
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc -O -package ghc -Wno-tabs nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `echo 1 2 3 | ./nibbles -simple #{testnbl} 2> /dev/null`
out == "36\n" or raise 'generated haskell produces wrong output %p' % out

puts 'pass simple integration test'
