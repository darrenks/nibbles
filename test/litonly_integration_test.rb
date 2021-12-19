test_program = "p \"asdf\""
test_file = 'integration_test'
testnbl = test_file+'.nbl'
`rm #{testnbl} out.hs 2> /dev/null`
`ghc nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `./nibbles #{testnbl} 2> /dev/null`
out == "\"asdf\"\n" or raise 'generated haskell produces wrong output %p' % out

puts 'pass lit only integration test'
