pass = true

# op tests
puts `runhaskell --ghc-arg=-Wno-tabs test/test.hs`
pass &&= $?.exitstatus==0

# parse speed test
puts `ruby test/parse_speed_test.rb 2>/dev/null`
p $?.exitstatus!=0 ? "FAIL parse speed" : "SUCCESS parse speed"
pass &&= $?.exitstatus==0

# integration test
puts `ruby test/integration_test.rb`
pass &&= $?.exitstatus==0

# input integration test
puts `ruby test/input_integration_test.rb`
pass &&= $?.exitstatus==0

# test generate quickref
puts 'generating quickref'
`runhaskell --ghc-arg=-Wno-tabs web/quickref.hs > web/site/quickref.html`
pass &&= $?.exitstatus==0

puts pass ? 'all tests pass' : 'FAIL'
