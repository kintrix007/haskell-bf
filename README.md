# haskell-bf

This is my Humble attempt at a rather inefficient Brainfuck interpreter
written in Haskell.

The reason I created this is that I wanted to see if monadic parsers
are a nice way to perform optimization on the AST. Although it turned out
to be a rather nice experience, it may be an overkill for something as
simple as Brainfuck.

The runtime is particularly inefficient, as it is using a tape
(`data Tape a = Tape [a] a [a]`) to represent the memory grid. This is a
decent way to implement a memory grid in Haskell, however it is not nearly as
efficient as an array would be in an imperative language.

In the future, I could look into unsafe Haskell to see if it possible to
speed it up that way. There are likely other ways than unsafe Haskell,
however I wanted to look into unsafe Haskell either way, so this would be
a good excuse.
