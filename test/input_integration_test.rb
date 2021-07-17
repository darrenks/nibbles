test_program = "_+_"
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `echo 1 2 3 | nibbles #{testnbl} 2> /dev/null`
out == "1\n2\n3\n6\n" or raise 'generated haskell produces wrong output %p' % out

puts 'pass input test'
