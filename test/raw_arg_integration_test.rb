test_program = '+2 $'
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc -O -package ghc nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `./nibbles #{testnbl} -- 3 4 2> /dev/null`
out == "5\n6\n" or raise 'generated haskell produces wrong output %p' % out

puts 'pass arg io integration test'
