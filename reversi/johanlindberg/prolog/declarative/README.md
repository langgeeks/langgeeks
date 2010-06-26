## Summary

We chose this kata as a starting point since it contains a little bit of everything. Some I/O, manipulating data structures and becuase you can solve it by using many different approaches. I think this was a good choice, at least for me.

I've tried Prolog before. I've read papers, blog posts, tutorials and books about it. I've tried the route finding and ancestor exercises that everyone else has but by doing this kata. I quickly went from thinking I knew (roughly) what was going on to understanding that I didn't. All in order to end up here.

I'm slightly confused. Mostly because implementing the solution was a lot easier than I thought it'd be and because several things that ended up in code was things I thought "What the hell. It might work, I'll try." about. I've *never* had that happen to me before (not even in Perl).

Once I got used to thinking in terms of pattern matching and realizing that I can put a variable *anywhere* (well, mostly) it got a lot easier. I also struggled a bit with how to read rule definitions and I'm not sure I've got a grip on that yet. I can not, for example, write the process and process_stream rules from scratch. But know enough to modify them for my purposes without messing them up.

All in all. Prolog is kind of cool. I'll keep exploring it for a little while longer. There are several concepts that I need to learn more about. Cuts for example. I've got a theory about them and how I think they work, but I expect it to be far too simplistic. The same probably goes for unification.

## Practical

### Setup

I've used [Gnu Prolog 1.3.0](http://www.gprolog.org/) running on a Linux installation to solve this kata. I'm not aware of having used any platform or gnu prolog specific things in my code. It *should* work on other setups as well.

### Running the code

    GNU Prolog 1.3.0
    By Daniel Diaz
    Copyright (C) 1999-2007 Daniel Diaz
    | ?- change_directory('~/Projects/langgeeks/reversi/johanlindberg/prolog/').
    change_directory('~/Projects/langgeeks/reversi/johanlindberg/prolog/').
    
    yes
    | ?- ['reversi.pl'].
    ['reversi.pl'].
    compiling /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/prolog/reversi.pl for byte code...
    /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/prolog/reversi.pl:23-27: warning: singleton variables [C] for process_stream/4
    /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/prolog/reversi.pl compiled, 175 lines read - 12715 bytes written, 51 ms
    
    (4 ms) yes
    | ?- ['test.pl'].
    ['test.pl'].
    compiling /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/prolog/test.pl for byte code...
    /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/prolog/test.pl compiled, 26 lines read - 3451 bytes written, 19 ms
    
    (4 ms) yes
    | ?- all.
    all.
    testing test1.txt pass.
    testing test2.txt pass.
    testing test3.txt pass.
    
    (64 ms) yes
    | ?- find_moves('test1.txt').
    find_moves('test1.txt').
    E3,D6,C5,F4
    
    true ? 
    
    
    (32 ms) yes
    | ?- 

### Resources

I have a copy of [The Art of Prolog](http://mitpress.mit.edu/catalog/item/default.asp?tid=8327&ttype=2), hoping it was going to be a good resource to get me through some practical Prolog programming. It was not. It is, however, a very good read on the theory of Logic programming and the concepts Prolog are built around.

The [Gnu Prolog Manual](http://www.gprolog.org/manual/gprolog.html) requires you to know quite a bit about and be familiar with the jargon of Logic programming and Prolog to be useful. I tried to use it to find out things like: *how do I load a .pl file?* without any luck.

[Wikibooks](http://en.wikibooks.org/wiki/Category:Prolog) on the other hand, turned out to be a very good practical resource with lots and lots of snippets of code and examples.
