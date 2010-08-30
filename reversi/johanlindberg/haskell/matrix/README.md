## Summary

Haskell is one of those languages I've been flirting with for years now. I've never tried it but I've read enough blog posts and articles to realize that I *should* learn it at some point. Now, is as good a time as any.

I'm not going to say it was easy. Because it wasn't. There were several concepts that I struggled with before I got to the point where I could at least produce a solution that compiles and does what I expect it to. Also, I *know* that there are many more concepts lurking in Haskell that I haven't even tried to get my head wrapped around.

I think it have helped that I did both Prolog and Erlang before Haskell because there are some similarities even though they're probably just artificial. I guess some of this FP magic is starting to sink in. I've managed to produce a solution even though it's probably not very Haskellish. So far the language has proved to be everything I've hoped for and more (lots of stuff unexplored).

Sometimes Haskell gave me the same kind of feeling that Prolog did. I would just try out some piece of code for the heck of it and it worked. If I've understood correctly you can run into some situations that will be really slow or not work at all (much like Prolog) but I've managed to stay away from that somehow and in general feel that I can produce running (albeit ugly) code.

Haskell gets 4+ thumbs up from me. I sure hope I can use it some more in the future.

## Practical

### Setup

I've used [Hugs 98](http://www.haskell.org/hugs/) and Haskell-mode for Emacs running on a Linux netbook for this kata.

### Running the code

    __   __ __  __  ____   ___      _________________________________________
    ||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard
    ||___|| ||__|| ||__||  __||     Copyright (c) 1994-2005
    ||---||         ___||           World Wide Web: http://haskell.org/hugs
    ||   ||                         Bugs: http://hackage.haskell.org/trac/hugs
    ||   || Version: September 2006 _________________________________________
    
    Haskell 98 mode: Restart with command line option -98 to enable extensions
    
    Type :? for help
    Hugs> :load "/home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/haskell/matrix/reversi.hs"
    Main> runTestTT tests
    Cases: 16  Tried: 16  Errors: 0  Failures: 0
    
    Main> findAllMoves "test1.txt"
    [(5,3),(2,4),(3,5),(4,2)]
    
    Main>
 
### Resources

I have been reading [Programming In Haskell](http://www.cs.nott.ac.uk/~gmh/book.html), which has taken me through some of the more basic stuff (syntax and such). I can't say I fully understand everything the book covers but I can fairly certain say that it wasn't enough to solve this kata. My approach of running before walking usually works out quit well because I can dig deeper in the areas I stumble on. Not so with this book since it lacks most everything slightly advanced.

I'v also bought [Real World Haskell](http://book.realworldhaskell.org/) but I haven't read that much of it. I've heard good things about [Learn You a Haskell](http://learnyouahaskell.com/) but I haven't had time to read much of that either.