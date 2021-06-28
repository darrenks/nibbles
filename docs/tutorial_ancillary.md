# Tutorial: Ancillary

In this tutorial you will learn some useful features to help you reach par in some code golf problems.

## Laziness
So you might have been wondering how using the input args work. When does it parse the input? What if I use both `@` and `` `3``? What if I use it multiple times? The answers are all very simple (if you understand laziness). Laziness is the whole reason Nibbles uses Haskell.

When does it parse the input? This is the wrong question to be asking in a purely functional language. But more practically speaking, your program won't get stuck waiting for input if you don't use any input variables. If your program unexpectedly hangs you may have accidentally used an input var, causing it to wait for input.

What if I use both `@` and `` `3``? They both work independently, sharing the same input.

What if I use it multiple times? It will return the same result each time. In fact everything in nibbles is a pure function.

### Infinite lists and errors
If this is all unfamiliar to you, you could read up on [lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation). But you'll do ok with just the information here, as laziness should be pure win compared to eager evaluation (for codegolf purposes). Anything you would do eagerly will still work in equal or better asymptotic time lazily.

Typical examples to show off laziness revolve around not throwing an error if you never use a value. For example `/1 0` throws an error but if we do something like `=\: /1 0 2 1` (which builds the list of `[error,2]`, reverses it then takes the first element), it never uses the value of `/1 0` and so therefore never errors.

And we could also generate the list of numbers from 1 to a googol, then select only the first 50 that are odd as such:
```
<50 & ,^10 100 %$2
```
Without it using all of the time the universe has to offer. This is useful when we don't know how many elements we will need at later stages of computations (typically languages have a separate concept of streams to support this, but that is superflous in lazy languages).

### Full laziness

Another useful thing about Haskell's laziness (full laziness). Is that expressions are never evaluated multiple times, even inside a loop. For example:
```
+,100000000
```
Computes the sum from 1 to 100,000,000, and takes 1.16 seconds on my computer not including compile time.
```
+ .,1000 +,100000000
```
Which computes the sum of that sum in a loop 1,000 times takes only 1.33 seconds. If you had done this in a strict language it would have taken 1,000 times longer. Yes some optimizing compilers in languages like C might have been smart enough to automatically move that computation out of the loop, but in general they cannot because their type system doesn't understand side effects.

This is critical for whole model of Nibbles' syntax (since there would be no easy way to move an expensive computation from inside a loop to out).

### Challenge Exercise
Write a program that finds one factor of composite number from stdin, let's say 3902309423233451. You may not hard code constants besides numbers <= 2.

<details>
  <summary>Solution</summary>
  
  ```
  <1 &          # Get the first 1 elements of the filtered list.
    >1,=@ 1     # Generate the list from 2 to input
    - 1 %=`3 1$ # \elem -> 1 - (input % elem)
  ```
  This is the first time we've needed comments, use them with `#`

  That was hard, and there are still some pain points we haven't learned how to get around yet, like having to extract the input number from a list twice (and differently even since there was something added to the context). The key thing for this lesson though is we generated a list up to the original input number which was guaranteed to contain a factor, but we didn't have to pay the computational cost of checking all numbers, nor did we need to explicitly exit the loop.
</details>

## Output

## More inputs

## Auto values

`~` Can be used to save space specifying the most common integer for operations. For example `+4~` -> `5` and `*4~` -> `8` (1 is most commonly added to things and 2 is most commonly multiplied by things, see the quickref for what the auto values are for each function). This may not be shorter in literate form, but numbers are actually multiple nibbles (see below).


## Let statements

`;` is a let statement and is somewhat special. It takes one argument and saves that argument for use by anything after it, you reference it in the same way you do for function arguments. For example `+ ;2 $` is the same as `+ 2 2`. Note that the scope of this variable is limited to the same scope as its highest level dependency. I.e. if you use a loop variable the let variable can only be used within that loop.


## Tuples

## Maybe

## Vectorization

creating functions
	recursive