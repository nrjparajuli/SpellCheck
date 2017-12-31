# SpellCheck
An implementation of spell checker with suggestions in Haskell

To compile the program, execute the following code:
$ ghc spellcheck.hs -o spellcheck

To run the program, execute the following code:
./spellcheck PATH-TO-DICTIONARY PATH-TO-TEST-FILE PATH-TO-RESUlT-FILE

If the result-file does not exist, this program will make one for you.
Also, notice that the source code comes with two implementations for finding distances
between two given words, both of which were implemented by us. One of the implementation is
based on the pseudocode we found in one of Stanford's Computer Science Course slides (referenced
in the source code). The other implementation is ours and is much much faster compared to the 
recursive implementation.
