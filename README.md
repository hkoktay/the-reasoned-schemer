# Code for The Reasoned Schemer


## Background

The Reasoned Schemer is a book about relational programming - logic programming
without side-effects. You don't need a computer to work through the book but I
recommend nevertheless you use a computer and a scheme system you are familiar
with.


## Summary

This is code for the book The Reasoned Schemer. Just like the code for
The Little Schemer and The Seasoned Schemer, the code is split into 10 scheme
source files, one for every chapter. If you use this code while reading
The Reasoned Schemer, I recommend to load the code function by function. The
reason is that functions in a chapter will be redefined later, so they are
different than the function definitions in the beginning of the chapter. So when
you load the complete chapter source file into your scheme repl and try an example
from the book with a redefined function, you might get an unexpected result. Thus
if you work through the book, don't load the chapter file in the repl, instead
evaluate the procedures of the chapter one by one in your repl.


## Additional Information

Functions defined in minikanren end with an 'o', for example 'caro' instead of
'car'. Scheme procedures have a signature as a comment which shows the used data
types. Compound data types and non-built-in data structures are shown in square
brackets. The data type ```any``` can be any scheme expression. For example the data
definition for an atom is in square brackets because it is not a built-in data
structure in scheme. As an example the "atom" data definition:

    An [atom] is
    - Number or
    - String or
    - Symbol

Here is the definition of the ```atom?``` function with the function signature as a
comment:

    ;; atom?: any -> boolean
    (define (atom? e)
       (and (not (list? e)) (not (pair? e))))

Note that scheme has strong dynamic types. Types are checked when you run the
program or call a function. If you use the wrong type the scheme implementation
returns an error when you run your program. A list of data type definitions used
in 'The Reasoned Schemer' can be found in the file NOTES. If the definition of a
function changes, it is annotated, with 1.revision, 2.revision etc.

The code comes with tests. Loading the scheme file of the chapter evaluates the
corresponding tests. If a test fails it prints a message to the current output
port showing the expected value and the computed value of the scheme
expression. For example the test ```(test "atom?" (atom? 'a) #f)``` prints:

    Testing "atom?"
    Failed: (atom? 'a)
    Expected: #f
    Computed: #t

The tests have two goals. One is to ensure that the code is correct, the other is
to show you examples how to apply the code. This is also the reason why the
tests are close to the defined function and not in a separate file.

The book uses different symbols for the succeed and fail procedures. The book
uses ```#s``` and ```#f``` while the minikanren implementation uses ```succeed``` and
```fail```.

The recommended scheme implementation for the code is Chez Scheme because of its
debugging features, good documentation and build-in expression editor
[2][3][4]. Start a scheme repl in the directory of the code and load
the chapter source file to test the code of the chapter.

    (load "chapter01.scm")

If you want to check all chapters execute the file ```run-tests.scm``` in the
directory of the code - needs Chez Scheme. Another option is a to execute this
bash one liner in the code directory:

    for f in chapter* ; do echo '(exit)' | scheme $f -q --; done
   
If you already tested the chapter code restart your scheme implementation to
clear the top-level environment. This is also recommended when you finished a
chapter and want to start with another chapter. You can find more information
about minikanren on the web [5][6][7].

Now get the book and start a wonderful journey into the world of relational
programming with miniKanren.



References:

[1] https://mitpress.mit.edu/books/reasoned-schemer-0

[2] https://github.com/cisco/ChezScheme

[3] http://www.cs.indiana.edu/chezscheme/debug/

[4] http://scheme.com/csug8/

[5] http://minikanren.org/

[6] http://webyrd.net/

[7] https://www.youtube.com/playlist?list=PLO4TbomOdn2cks2n5PvifialL8kQwt0aW
