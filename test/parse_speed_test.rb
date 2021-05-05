# Times should all be < 0.2 seconds (on my computer at least)
# non first choice multi character op is 0.6 on reading binary but that is due to the haskell code generation.

# I believe most time is spent building the hs/nib/lit code due to repeated concatenation of lists. This could be done more efficiently but it's still much faster than the Haskell compiler.

# todo this is only really testing it from binary since literate form has different names now

n=1000
t=Time.now
`
# ghc nibbles.hs 2> /dev/null
echo nested expressions 1>&2
time echo #{'+'*n+'1 '*n} 1 | nibbles -c
time nibbles a.nbb

echo flat expressions 1>&2
time echo #{'+1'*n} 1 | nibbles -c
time nibbles a.nbb

# This could be a sort if operand is a list, if this isn't memoized this could become exponentially slow

echo non first choice multi character op 1>&2
time echo '#{'//'*n + '1 1 '*n} 1' | nibbles -c
time nibbles a.nbb

echo first choice multi character op 1>&2
time echo '#{'//'*n},3' | nibbles -c
time nibbles a.nbb

echo long string 1>&2
time echo '"#{'a'*n}"' | nibbles -c
time nibbles a.nbb

echo long number 1>&2
time echo '#{'1'*n}' | nibbles -c
time nibbles a.nbb
`
puts Time.now - t > 3 ? 'fail parse too slow' : 'pass parse speed test'