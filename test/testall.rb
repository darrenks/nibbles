pass = true

# op tests
puts `ghc -package ghc test/test.hs && test/test`
pass &&= $?.exitstatus==0

# parse speed test
puts `ruby test/parse_speed_test.rb` # 2>/dev/null`
p $?.exitstatus!=0 ? "FAIL parse speed" : "SUCCESS parse speed"
pass &&= $?.exitstatus==0

puts `ruby test/integration_test.rb`
pass &&= $?.exitstatus==0

puts `ruby test/input_integration_test.rb`
pass &&= $?.exitstatus==0

puts `ruby test/simple_integration_test.rb`
pass &&= $?.exitstatus==0

puts `ruby test/arg_io_integration_test.rb`
pass &&= $?.exitstatus==0

puts `ruby test/litonly_integration_test.rb`
pass &&= $?.exitstatus==0

puts `ruby test/utf8_integration_test.rb`
pass &&= $?.exitstatus==0

# test generate quickref (not in distributed code)
if Dir.exist?('web')
   puts 'testing quickref compile'
   `ghc web/quickref.hs 2> /dev/null`
   puts "ERROR: Quickref failed to compile!" if $?.exitstatus!=0
   pass &&= $?.exitstatus==0

   `sh scripts/generate`
end

puts pass ? 'all tests pass' : 'FAIL' + "#"*100
