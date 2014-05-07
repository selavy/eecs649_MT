#lang racket

(require racket/trace)

(define articles '(the a))
(define nouns '(block bird cat dog hill))
(define adjectives '(small big scary tall pretty))
(define verbs '(block flies runs))
(define prepositions '(over to up))

(define DB '( ((noun block) bloc) ((verb block) bloque) ((noun cat) chat) ((article the) le) ((adjective big) grand) ((adjective scary) effrayant) ((noun dog) chien) ((verb runs) court) ((preposition up) jusque à) ((noun hill) colline) ))

(define (article? word) (if (member word articles) #t #f))
(define (noun? word) (if (member word nouns) #t #f))
(define (adjective? word) (if (member word adjectives) #t #f))
(define (verb? word) (if (member word verbs) #t #f))
(define (preposition? word) (if (member word prepositions) #t #f))

(define (mark-up sent)
  (printf "Marking up: ~a.\n" sent)
  (let* ([first-word (car sent)] [rest-of-sentence (cdr sent)])
  (cond
    ((article? first-word) (cons (list 'article first-word) (noun-phrase+verb-phrase? rest-of-sentence)))
    ((noun-phrase+verb-phrase? rest-of-sentence))
    (else #f))))

(define (noun-phrase+verb-phrase? clause)
  (let ([word (car clause)])
  (cond
    ((null? clause) #f)
    ((not (pair? clause)) #f)
    ((noun? word) (cons (list 'noun word) (verb-phrase? (cdr clause))))
    ((adjective? word) (cons (list 'adjective word) (noun-phrase+verb-phrase? (cdr clause))))
    (else #f)
  )))

(define (verb-phrase? clause)
  (let* ([word (car clause)] [verb (verb? word)] [prep (and (pair? (cdr clause)) (preposition? (cadr clause)))])
  (cond
    ((null? clause) #f)
    ((not (pair? clause)) #f)
    ((and (> (length clause) 2) verb prep) (list (list 'verb word) (list 'preposition (cadr clause)) (object-phrase (cddr clause))))
    ((and (> (length clause) 1) verb) (list (list 'verb word) (object-phrase (cdr clause))))
    ((and (eq? (length clause) 1) (verb? word)) (list (list 'verb word)))
    (else #f))))
    
(define (adj+obj? clause)
  (let ([word (and (pair? clause) (car clause))])
    (cond
      ((null? word) #f)
      ((noun? word) (list 'noun word))
      ((not (pair? clause)) #f)
      ((adjective? word) (list (list 'adjective word) (adj+obj? (cdr clause))))
      (else #f)
    )))

(define (object-phrase clause)
  (let ([word (car clause)])
    (cond
      ((null? clause) #f)
      ((not (pair? clause)) #f)
      ((article? word) (list (list 'article word) (adj+obj? (cdr clause))))
      (else #f)
    )))

(define (simple-subst word)
  (let ([sub (and (pair? word) (assoc word DB))])
    (if sub (cdr sub) #f)))

(define (translate sent)
  (let ([ssub (simple-subst sent)])
    (if ssub ssub
        (flatten (append-map
                  (λ(phrase)
                    (cond
                      [(simple-subst phrase)]
                      [(not (pair? phrase)) '()]
                      [(append-map (λ(sub-phrase) (translate sub-phrase)) phrase)]
                      [else '()]
                      ))
                  sent)))))

(translate (mark-up '(the big block block the big scary dog)))
    