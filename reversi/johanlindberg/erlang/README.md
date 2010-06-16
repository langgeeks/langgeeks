## Summary

After having finished the Prolog version of the reversi kata a little sooner than I had originally thought. I decided to try it out with Erlang as well. Erlang and Prolog are related (it shows in the syntax) and I was hoping that reading about one language would explain bits and pieces in the other as well.

I've never tried Erlang before. I've heard and read about it. I even bought Programming Erlang a couple of years ago. But I had never tried writing any code or putting an effort into thinking about how it does what it does. I must say, it was a pleasant experience. More so than I thought. Perhaps it's because I just came from Prolog but Erlang felt very good for, mainly, three reasons. 1) namespaces, 2) functions return values, 3) batteries included. Doesn't take much to please me, does it? ;-)

Having done Prolog first sure helped. I never struggled particularly much with syntax. There was always another option if the code didn't do what I wanted (if/case for example). This is both a pro and a con in comparison to Prolog. In Prolog, if something works, you know you've got it right. In Erlang, I wrote and re-wrote bits and pieces of code trying out a lot of diferent variations without any strong feeling on what to settle on. But this kind of thing takes time in any language so I'm not particularly worried.

Also, I wrote all of the code from scratch this time. Not relying on examples or cookbooks as I did with Prolog.

I'll definitely keep using Erlang. I've got another piece of code I'd like to try it out on. Perfect for testing it's concurrency capabilities. Next up I intend to look closer at Mnesia (DB) and parallelization of programs.

## Practical

### Setup

I've used [Erlang R13B03](http://www.erlang.org/) running on a Linux installation to solve this kata. I'm not aware of having used any platform specific things in my code. It *should* work on other setups as well.

### Running the code

    Erlang R13B03 (erts-5.7.4) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.7.4  (abort with ^G)
    1> pwd().
    /home/johanlindberg
    ok
    2> cd("Projects/langgeeks/reversi/johanlindberg/erlang").
    /home/johanlindberg/Projects/langgeeks/reversi/johanlindberg/erlang
    ok
    3> c(reversi).
    {ok,reversi}
    4> c(reversi_tests).
    {ok,reversi_tests}
    5> reversi:test().
      All 5 tests passed.
    ok
    6> reversi:find_moves("test1.txt").
    [{5,3},{2,4},{3,5},{4,2}]
    7>

### Resources

I've been reading bits and pieces of [Programming Erlang](http://pragprog.com/titles/jaerlang/programming-erlang). Can't say much about it though since I've mostly used it as a reference.