# Times should all be < 0.2 seconds (on my computer at least)
# non first choice multi character op is 0.6 on reading binary but that is due to the haskell code generation.

# I believe most time is spent building the hs/nib/lit code due to repeated concatenation of lists. This could be done more efficiently but it's still much faster than the Haskell compiler.

n=800
pass = true
`ghc nibbles.hs 2> /dev/null`; pass &&= $?.exitstatus==0

t=Time.now
`echo nested expressions 1>&2
time echo #{'+'*n+'1 '*n} 1 | nibbles -c` ; pass &&= $?.exitstatus==0
`time nibbles a.nbb` ; pass &&= $?.exitstatus==0

`echo flat expressions 1>&2
time echo #{'+1'*n} 1 | nibbles -c` ; pass &&= $?.exitstatus==0
`time nibbles a.nbb` ; pass &&= $?.exitstatus==0

# This could be a sort if operand is a list, if this isn't memoized this could become exponentially slow (only the binary version will fail since literate uses st)

`echo non first choice multi character op 1>&2
time echo '#{'//'*n + '1 1 '*n} 1' | nibbles -c`; pass &&= $?.exitstatus==0
`time nibbles a.nbb` ; pass &&= $?.exitstatus==0

`echo first choice multi character op 1>&2
time echo '#{'st'*n},3' | nibbles -c`; pass &&= $?.exitstatus==0
`time nibbles a.nbb` ; pass &&= $?.exitstatus==0

`echo long string 1>&2
time echo '"#{'a'*n}"' | nibbles -c` ; pass &&= $?.exitstatus==0
`time nibbles a.nbb` ; pass &&= $?.exitstatus==0

`echo long number 1>&2
time echo '#{'1'*n}' | nibbles -c`; pass &&= $?.exitstatus==0
`time nibbles a.nbb`; pass &&= $?.exitstatus==0

raise 'errors in runs' if !pass
raise 'fail, parse too slow' if Time.now - t > 3
puts 'pass parse speed test'
