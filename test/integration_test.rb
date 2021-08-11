# test nibbles on a program which computes all primes < 400 then prints 62nd
test_program = "= 63-,*; 20$+>~.,$>~*$,/*@@$\n"
test_file = 'integration_test'
testnbl = test_file+'.nbl'
testnbb = test_file+'.nbb'
`rm #{testnbl} #{testnbb} out.hs 2> /dev/null`
`ghc nibbles.hs 2> /dev/null`
File.open(testnbl,'w'){|f|f<<test_program}

out = `nibbles #{testnbl} 2> /dev/null`
out == "293\n" or raise 'generated haskell produces wrong output %p' % out

`nibbles -c #{testnbl} 2> /dev/null`
file=File.open(testnbb,"rb")
out = file.read.size 
out == 14 or raise 'binary size is wrong %p' % out

out = `nibbles -e #{testnbb}`
out == test_program or raise 'decompiled program differs %p' % out

puts 'pass integration test'
