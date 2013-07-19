(module trie 
  (mk-trie search prefix? subtrie insert! copy-trie trie->list
           ;; same functions as above, accepting strings instead of lists
           search-string prefix-string? subtrie-string insert-string!)
  (import scheme chicken)

  (define-record trie content children)

  ;; a MUTABLE implementation of tries.

  ;; mk-trie :: Trie[A]
  ;; creates an empty trie. 
  (define (mk-trie) (make-trie (list) (list)))

  ;; search :: List[A] -> Trie[A] -> Bool
  ;; return #t if the item is present in the trie.
  ;; For example: if the trie contains "string" and "item" prefix? returns
  ;; #t for string, #t for item
  ;; #f for str, #f for i
  (define (search lst trie)
    (search-helper lst trie #f))

  ;; prefix :: List[A] -> Trie[A] -> Bool
  ;; return #t if the item is a prefix of an item in the trie
  ;; For example: if the trie contains "string" and "item"
  ;; prefix? returns
  ;; #t for str, #t for string, #t for i ...
  ;; #f for strong
  (define (prefix? lst trie)
    (search-helper lst trie #t))

  ;; search-helper :: List[A] -> Trie[A] -> Bool -> Bool
  ;; search an element in a trie. The element should be a list of
  ;; whatever is saved in the trie. Returns #t or #f.
  ;; if prefix? is true, returns #t even if the match is partial
  (define (search-helper lst trie prefix?)
    (if prefix?
      (not (not (subtrie lst trie #f)))
      (equal? '() (subtrie lst trie #f))))

  ;; subtrie :: List[A] -> Trie[A] -> Trie[A]
  ;; subtrie :: List[A] -> Trie[A] -> Bool -> Trie[A]
  ;; returns the subtrie rooted at the last element of lst.
  ;; for example, if the trie contains abcd and abef
  ;; subtree ab returns a trie, with b as root element
  ;; containing "cd" and "ef". This function is useful for, e.g. 
  ;; autocompletion.
  ;; This function takes an optional parameter indicating whether
  ;; the subtrie should be a copy (if true) or a part of the 
  ;; original trie (if false), defaults to #t.
  (define (subtrie lst trie . copy)
    (let subtrie-helper ((lst lst) (trie trie) (copy (optional copy #t)))
      (cond ((and (null? (trie-children trie)) (null? lst)) '())
            ((null? (trie-children trie)) #f)
            ((null? lst) 
             (if copy
               (copy-trie trie)
               trie))
            (else
              ;; search for the right subtrie
              (let loop ((xs (trie-children trie)))
                (cond ((null? xs) #f)
                      ((equal? (trie-content (car xs)) (car lst))
                       (subtrie-helper (cdr lst) (car xs) copy))
                      (else (loop (cdr xs)))))))))


  ;; copy-trie :: Trie[A] -> Trie[A]
  ;; returns a copy of the trie. The copy doesn't share any structure
  ;; with the original.
  (define (copy-trie t)
    (make-trie (trie-content t) 
               (map (lambda (x) (copy-trie x))
                    (trie-children t))))

  (define (listify x)
    (if (list? x) x (list x)))

  ;; trie->list :: Trie[A] -> List[List[A]]
  ;; returns a list of all the content of the trie.
  ;; Example: the trie contains "str" and "tst"
  ;; trie->list will return ((#\t #\s #\t) (#\s #\t #\r))
  ;; Notice that this function won't return a list of strings
  ;; since there's no way to know wether we originally used
  ;; insert-string! or insert!.
  (define (trie->list t)
    (if (null? (trie-children t))
      (list (trie-content t))
      (map (lambda (x) (append (listify (trie-content t)) (listify x)))
           (apply append (map trie->list (trie-children t))))))

  ;; add-child! :: Trie[A] -> A -> Trie[A]
  ;; returns a modified trie, with the given element added as child
  ;; the only mutations to the structure are performed here.
  (define (add-child! position content)
    (let ((new-node (make-trie content (list))))
      (trie-children-set! position (cons new-node (trie-children position)))
      new-node))

  ;; search-ref-same-level :: A -> List[Trie[A]] -> Trie[A] | Nil
  ;; search in a list of children the (only) node that has the given
  ;; content. If there isn't such element, returns '()
  (define (search-ref-same-level element trie-children-list)
    (cond ((null? trie-children-list) '())
          ((equal? (trie-content (car trie-children-list)) element) 
           (car trie-children-list))
          (else (search-ref-same-level element (cdr trie-children-list)))))

  ;; insert! :: List[A] -> Trie[A] -> #t
  ;; inserts the given element in the trie. Returns #t
  (define (insert! lst trie)
    (cond ((null? lst) #t)
          ((null? (trie-children trie)) 
           (let ((new-node (add-child! trie (car lst))))
             (insert! (cdr lst) new-node)))
          (else
            (let ((ref (search-ref-same-level (car lst) (trie-children trie))))
              (if (equal? ref '())
                (let ((new-node (add-child! trie (car lst))))
                  (insert! (cdr lst) new-node))
                (insert! (cdr lst) ref))))))

  ;; some convenience function to insert/search/... strings in the trie
  ;; (FUNCTION-string str trie) is equivalent to 
  ;; (FUNCTION (string->list str) trie)

  ;; insert-string! :: String -> Trie[Char] -> #t
  (define (insert-string! str trie)
    (insert! (string->list str) trie))

  ;; search-string :: String -> Trie[Char] -> Bool
  (define (search-string str trie)
    (search (string->list str) trie))

  (define (prefix-string? str trie)
    (prefix? (string->list str) trie))

  (define (subtrie-string str trie . copy)
    (subtrie (string->list str) trie (optional copy #t)))
  )
