# Why?

### Why code golf?
That's outside the scope of this page, but my quick answer is that can be fun to produce surprisingly short and incomprehensible code. It's also good practice to push yourself to seek simpler solutions than your first instinct.

### Why code golf languages?
This too is outside the scope of this page, but my quick answer is that it is fun to produce even shorter and more incomprehensible code! It also can make golfing more about seeking the true simplest solution to a problem rather than fighting your mainstream language to avoid long function names and abuse obscure features.

## Why Nibbles?
-	To have a language that I would actually want to golf in.
	-	This means simple but also competitive.
-	To make the language Golfscript should have been.

### Some history
I created [Golfscript](http://www.golfscript.com/golfscript/) in 2007. Originally it was an experiment just to see what a language would be like if the only control structure was string evaluation (because that was so powerful in Ruby golf). A stack was added as a simple way to increase its power. Then I realized that if code strings had their own type (blocks) then more overloading was possible. This made it pretty similar to other stack based languages. But as a golfer, I focussed on the fact that it was actually very good at code golf, so I made it even better by adding more operations and the result was the first ever "golf language". Although this is arguable as APL has always prized short code, but APL was optimized for the real world and doesn't fair as well on toy problems.

Sure Golfscript was a little hacky and had its problems but it enjoyed being widely used because it had the shortest solutions for code golf problems (by far). It had a monopoly, being the only language that was optimized for this.

But Golfscript was never fully optimized for truly short solutions, after all it really only used the symbol characters (of which there are only 32), yet stored instructions in bytes. I did this because I thought it would be unfair and impure to do otherwise, its goal was to beat mainstream languages, but clearly this could have been done by just compressing the source code of programs said languages.

The allure of creating even more compact golf languages was irresistible and over time many have been created it. But compactness was usually accomplished by adding more instructions instead of reducing the instruction size. The result of this was very complicated languages (competitive ones have over 400 different built-ins).

This has bothered me because

1.	Solutions will rely on specialty functions for common golf themes (e.g. palindromize, factor, pi digits, etc.), rather than solving the problem yourself, as you would in a normal language.
1.	Golf became more about hunting for puzzle pieces than manipulating puzzle pieces (searching through a long list of arbitrary functions rather than problem solving).

## Philosophy of Nibbles

Be simple **and** good at code golf!

And some strategies that were chosen to accomplish this:

1.	Decrease size by decreasing instruction size rather than adding more instructions.
	-	This is more to keep it simple than to be optimal, more instructions would actually be shorter (if they are well chosen).
	-	Half bytes keep it feeling like real golf, prefer it over variable bits
1.	Functional programs are shorter and more intuitive.
	-	Bonus is you get compatibility with laziness.
1.	Overloading by type should only be used with static typing.
	-	Changing types would almost never be what you actually want if there is overloading by type.
1.	Prefix notation with DeBruijn indices are easy to use and eliminate the need for all function delimiters and stack manipulation.
	-	Tacit is beautiful, but I couldn't find a way to match this efficiency while keeping it this simple.
1.	There should be only 1 way to do things and 0 ways to do useless things, otherwise wastes entropy.

### Anti-philosophy of Nibbles

Be good at everything.

It seems like a no-brainer to automatically **compress strings**, but doing so adds complexity to the golfing process and also makes the golf language quite arbitrary (do we optimize for Spanish too, what about Chinese or Mayan?). Sure, without compression we'll be at a disadvantage in problems that include printing arbitrary english sentences, but I'd argue those weren't great problems for golf in the first place.

Currently there are no **floating point** numbers. In the past I thought they didn't belong in code golf because of issues like difference between languages in what precision to use. However the rules on stackexchange don't require specific output so this wouldn't be as much of an issue. None-the-less, I haven't thought enough about how to fit them into the ops table and how to support the many more operations that would be needed for them. And they aren't useful for that high of a percent of problems, so for at least for the foreseeable future, Nibbles just isn't the language to use if you want to do floating point math.

**Regexes** are cool and undoubtedly better than Nibbles at some types of string manipulation. However they are complicated and really a language of their own.

### Future

My hope is that despite its additional goal of being simple, Nibbles is able to beat the complicated golf languages in enough holes that it is widely adopted. But even if successful, I realize that the future will repeat itself, the allure of creating even shorter languages will persist and will be realized. For example let's say you took the ideas of Nibbles, and coded each op with a variable number of bits based on how likely it was used (or even fractional bits with arithmetic coding). You could also add many more instructions, even rarely used ones. Clearly this language would be more compact.

But I also hope that this allure is weaker, and that people prefer to golf in simpler languages. And that other simple languages are created. What if you took the ideas of Jelly or Husk (partial application) and fit them into 4ish bit ops? I bet that would be fun and I hope to see that some day.
