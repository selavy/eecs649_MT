#lang racket
(require racket/trace)
; notes
;
; Q: Is it possible to do a 2-pass program, first pass tries simple substitution if there
; is only 1 choice, and the second pass tries to parse type of speech and gender to narrow
; down choices?
;
; A: I don't think that will work becuase how will I get correct word order?
;
; Q: How should I represent the knowledge that I gain about the function of the words in
; the sentence?  Right now I am using (propery (property (part-of-speech word))), etc. But
; is there a better way?

(define nouns '((block blocks) (cat cats) (dog dogs)))
(define verbs '((is are) (run runs) (eats eat))) ; get rid of is/are?
(define articles '(the a an))
(define adjectives '(big small black white))
(define fr_noms '((bloc blocs) (chat chats) (chien chiens)))
(define fr_verbes '(((court courent) (mange mangent))))
(define fr_articles '((le la) (les les) (un une)))
(define fr_adjectives '((grand grande) (petit) (petite) (noir noire) (blanc blanche)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIMPLE SUBSTITUTION TRANSLATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define DB '((block bloc) (cat chat) (dog chien) (is est) (big grand) (black noir) (white blanc) (the le) (a un) (small petit) (run court) (eat mange)))
(define (simple-subst sent)
  (map (λ(word) (cadr (assoc word DB))) sent))
;(simple-subst '(the block is black))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-noun? w)
  (ormap (λ(x)
           (cond
             ((eq? w (first x)) (list 'singular (list 'noun w)))
             ((eq? w (last x)) (list 'plural (list 'noun w)))
             (else #f)))
         nouns))

(define (is-verb? w)
  (ormap (λ(x)
           (cond
             ((eq? w (first x)) (list 'third-person (list 'singular (list 'verb w))))
             ((eq? w (last x)) (list 'third-person (list 'plural (list 'verb w))))
             (else #f)))
         verbs))

(define (is-article? w)
  (ormap (λ(x) (if (eq? w x) (list 'article w) #f)) articles))

(define (is-adjective? w) (let ((l (member w adjectives))) (if l (list 'adjective (first l)) #f)))

;(define (mark-up sent)
;  (list (list 'noun-phrase (is-article? (first sent)) (is-noun? (cadr sent))) (list 'verb-phrase (is-verb? (caddr sent)) (is-adjective (cadddr sent)))))

(define (noun-and-or-adj? p)
  (let ([adj? (and (pair? p) (is-adjective? (first p)))])
  (cond
    ((and (pair? p) (is-noun? (car p))))
    (adj? (list adj? (noun-and-or-adj? (cdr p))))
    (else #f))))

(define (is-noun-phrase? p)
  (let* ([art (is-article? (first p))] [noun (is-noun? (first p))] [adj (is-adjective? (first p))])
  (cond
    (art (and (pair? p) (let ([noun (noun-and-or-adj? (cdr p))]) (list art noun))))
    (noun)
    (adj)
    (else #f)
     )))

(define (mark-up sent)
  (cond
    ((is-noun-phrase? sent))
    (else #f)))

;(define (mark-up sent)
;  (cond
;    ((is-noun? sent))
;    ((is-verb? sent))
;    ((is-adjective? sent))
;    ((is-article? sent))
;    ((is-noun? (first sent)) (printf "Is noun\n"))
;    ((is-verb? (first sent)) (printf "Is verb\n"))
;    ((is-adjective? (first sent)) (printf "Is adjective\n"))
;    ((is-article? (first sent)) (printf "Is article\n"))
;    (else #f))
;  )

;(mark-up '(the big cat eats the small black dog))
(trace noun-and-or-adj?)
(mark-up '(the big black dog))