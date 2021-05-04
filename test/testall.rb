pass = true

# op tests
puts `runhaskell --ghc-arg=-Wno-tabs test/test.hs`
pass &&= $?.exitstatus==0

# parse speed test
puts `ruby test/parse_speed_test.rb 2>/dev/null`
pass &&= $?.exitstatus==0

# integration test
puts `ruby test/integration_test.rb`
pass &&= $?.exitstatus==0

# test generate quickref
puts 'generating quickref'
`runhaskell --ghc-arg=-Wno-tabs web/quickref.hs > web/site/quickref.html`
pass &&= $?.exitstatus==0

puts pass ? 'all tests pass' : 'FAIL'
