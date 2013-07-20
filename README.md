Trie
====
A simple (and mutable) trie implementation for Chicken scheme. 

Usage
-----
    
There are 7 main functions:

    (mk-trie)

Creates an empty trie.

    (insert! DATA TRIE)

Insert the data in the trie. DATA must be a list.

    (search DATA TRIE)

Search for the data in the trie. Returns #t if the item is present, but doesn't search for prefixes. 

    (prefix? DATA TRIE)

Returns #t if the first argument is the prefix of some data saved in the trie.

    (subtrie DATA TRIE (COPY #t))

Returns the subtrie rooted in the last element of PREFIX. If COPY is #t (the
default), it returns a copy of the trie. Otherwise, it returns the original
trie (meaning that you can modify it, and the modifications will be visible in
TRIE).

    (trie->list TRIE)

Linearize the trie. Notice that this function won't return a list of strings,
since there is no way to know whether you inserted a list of characters or a
string.  

    (copy-trie TRIE)

Returns a copy of the trie. The resulting trie will not share any structure
with the original

In all the previous functions, DATA must be a list.

The module exports 4 convenience functions that take a string instead of a
list:

    (insert-string! STR TRIE)
    (search-string STR TRIE)
    (prefix-string? STR TRIE)
    (subtrie-string STR TRIE (COPY #t))

Tests
-----
Tests are available in the file `test.scm`.

Mutable?
--------

Yes. Now everyone is in love with immutability and pure functions, and they're
right! However, tries are a data structure that is designed to keep large
quantities of data, and as such, it's not suited for the purely functional
world, since it would require a lot of copying (especially if the compiler is
not smart enough (TM)).

License
-------

MIT license
