(module trie (mk-trie search insert! insert-string! search-string)
  (import scheme chicken)

  (define-record trie content children)

  ;; a MUTABLE implementation of tries.

  ;; mk-trie :: Trie[A]
  ;; creates an empty trie
  (define (mk-trie) (make-trie (list) (list)))

  ;; search :: List[A] -> Trie[A] -> Bool
  ;; search an element in a trie. The element should be a list of
  ;; what's saved in the trie. Returns #t or #f.
  (define (search lst trie)
    (cond ((and (null? (trie-children trie)) (null? lst)) #t)
          ((null? (trie-children trie)) #f)
          ((null? lst) #t)
          (else
            (let loop ((xs (trie-children trie)))
              (cond ((null? xs) #f)
                    ((equal? (trie-content (car xs)) (car lst))
                     (search (cdr lst) (car xs)))
                    (else (loop (cdr xs))))))))

  ;; add-child! :: Trie[A] -> A -> Trie[A]
  ;; returns a modified trie, with the given element added as child
  (define (add-child! position content)
    (let ((new-node (mk-trie)))
      (trie-children-set! position (cons new-node (trie-children position)))
      (trie-content-set! new-node content)
      (trie-children-set! new-node (list))
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

  ;; insert-string! :: String -> Trie[Char] -> #t
  ;; convenience function to insert a string into the trie.
  ;; equivalent to (insert! (string-> list str) trie)
  (define (insert-string! str trie)
    (insert! (string->list str) trie))

  ;; search-string :: String -> Trie[Char] -> Bool
  (define (search-string str trie)
    (search (string->list str) trie))
)
