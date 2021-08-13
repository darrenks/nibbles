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

# test generate quickref (not in distributed code)
if Dir.exist?('web')
	puts 'testing quickref compile'
	`ghc -Wno-tabs web/quickref.hs 2> /dev/null`
	puts "ERROR: Quickref failed to compile!" if $?.exitstatus!=0
	pass &&= $?.exitstatus==0
end

puts pass ? 'all tests pass' : 'FAIL'
