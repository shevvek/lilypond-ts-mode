(module-define! (resolve-module '(geiser doc))
                'lily-mod
                (current-module))
(set-current-module (resolve-module '(geiser doc)))
(use-modules (srfi srfi-11)
             (ice-9 and-let-star))

(let ((old-arguments arguments))
  (set! arguments
        (lambda (proc)
          (or (old-arguments proc)
              (ly-primitive-args proc)))))

(define (ly-primitive-args proc)
  (and-let* (((program? proc))
             (doc (object-documentation proc))
             (arities (program-arguments-alist proc))
             (arg-names (map (lambda (m)
                               (string->symbol
                                (match:substring m 1)))
                             (list-matches "SCM ([^ ,()]+)"
                                           doc)))
             (req-count (length (assq-ref arities 'required)))
             (opt-count (length (assq-ref arities 'optional)))
             (kw-count (length (assq-ref arities 'keyword)))
             (rest-count (if (assq-ref arities 'rest) 1 0))
             ((= (length arg-names)
                 (+ req-count opt-count kw-count rest-count))))
    (let*-values
        (((req-args rest) (split-at arg-names req-count))
         ((opt-args rest) (split-at rest opt-count))
         ((kw-args rest) (split-at rest kw-count))
         ((rest) (and (pair? rest) (car rest)))
         )
      `(((required . ,req-args)
         (optional . ,opt-args)
         (keyword . ,kw-args)
         (rest . ,rest))))))

(set-current-module lily-mod)
