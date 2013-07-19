(module test (run-all)
  (import trie r5rs chicken)

  (define (run-all)
    (test1)
    (test2)
    (test3)
    (test4)
    (test5)
    (test6)
    (test7))

  (define (test1)
    (define trie (mk-trie))
    (assert (equal? #f (search-string "string" trie)))
    (assert (equal? #t (search-string "" trie))))

  (define (test2)
    (define trie (mk-trie))
    (insert-string! "string" trie)
    (assert (equal? #t (search-string "string" trie)))
    (assert (equal? #t (prefix-string? "string" trie)))

    (assert (equal? #f (search-string "str" trie)))
    (assert (equal? #t (prefix-string? "str" trie)))
    (assert (equal? #f (prefix-string? "stra" trie)))

    (assert (equal? #f (search-string "string " trie)))
    (assert (equal? #f (search-string "notpresent" trie))))

  (define (test3)
    (define trie (mk-trie))
    (insert-string! "string1" trie)
    (insert-string! "string2" trie)
    (insert-string! "anotherstring" trie)
    (assert (equal? #t (search-string "string1" trie)))
    (assert (equal? #t (prefix-string? "str" trie)))
    (assert (equal? #t (search-string "string2" trie)))
    (assert (equal? #t (search-string "anotherstring" trie)))
    (assert (equal? #f (search-string "another string" trie)))
    (assert (equal? #f (search-string "string3" trie))))

  (define (test4)
    (define trie (mk-trie))
    (insert! '(#\s #\t #\r #\i #\n #\g) trie)
    (assert (equal? #t (search-string "string" trie)))
    (assert (equal? #f (search-string "notpresent" trie)))
    (assert (equal? #f (search '(n o t i n) trie)))
    (assert (equal? #f (search '(#\n #\o #\t) trie)))
    (assert (equal? #f (search '(s t r i n g) trie)))
    (assert (equal? #t (search '(#\s #\t #\r #\i #\n #\g) trie))))

  (define (test5)
    (define trie1 (mk-trie))
    (insert-string! "str" trie1)
    (assert (equal? '((#\s #\t #\r)) (trie->list trie1)))
    (define trie2 (copy-trie trie1))
    (assert (equal? #t (prefix-string? "str" trie2)))
    (insert-string! "another" trie1)
    (assert (equal? #t (search-string "another" trie1)))
    (assert (equal? #f (search-string "another" trie2)))

    (insert-string! "stillother" trie2)
    (assert (equal? #f (search-string "stillother" trie1)))
    (assert (equal? #t (search-string "stillother" trie2)))
    (assert (equal? #f (search-string "other" trie1)))
    (assert (equal? #f (search-string "other" trie2))))

  (define (test6)
    (define trie1 (mk-trie))
    (insert-string! "1234" trie1)
    (insert-string! "1256" trie1)
    (define trie2 (subtrie-string "12" trie1 #t))
    (assert (equal? #t (search-string "34" trie2)))
    (assert (equal? #t (search-string "56" trie2)))
    (assert (equal? #f (search-string "1" trie2)))
    (assert (equal? #f (search-string "2" trie2))))

  (define (test7)
    (define trie1 (mk-trie))
    (insert-string! "1234" trie1)
    (insert-string! "1256" trie1)
    (define trie2 (subtrie-string "12" trie1 #f))
    (assert (equal? #t (search-string "34" trie2)))
    (assert (equal? #t (search-string "56" trie2)))
    (assert (equal? #f (search-string "1" trie2)))
    (assert (equal? #f (search-string "2" trie2)))
    ;; everything works as before, now let's insert some string in the subtrie
    (insert-string! "456" trie2)
    (assert (equal? #t (search-string "456" trie2)))
    ;; the prefix gets automatically inserted
    (assert (equal? #t (search-string "12456" trie1)))
    (assert (equal? #t (search-string "34" trie2)))
    (assert (equal? #f (search-string "34" trie1)))

    (insert-string! "987" trie1)
    (assert (equal? #t (search-string "987" trie1)))
    (assert (equal? #f (search-string "987" trie2)))

    (assert (equal? #f (search-string "78" trie2)))
    (insert-string! "1278" trie1)
    (assert (equal? #t (search-string "1278" trie1)))
    (assert (equal? #t (search-string "78" trie2))))
)

