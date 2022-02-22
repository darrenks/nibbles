test_program = '"a♯b"'
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc -O -package ghc nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `./nibbles #{testnbl} 2> /dev/null`
out == "a♯b\n" or raise 'generated haskell produces wrong output %p' % out

`./nibbles -c #{testnbl} 2> /dev/null`
file=File.open(testnbb,"rb")

out = `./nibbles -e #{testnbb}`
out.strip == '"a\\9839b"' or raise 'decompiled program differs %p' % out

puts 'pass utf8 integration test'
