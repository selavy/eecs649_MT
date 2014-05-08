; Peter Lesslie
; 2208496
; EECS 649
;
; Basic Translator using sentence structure to approximate sentence meaning.
;
; Machine translation and natural language processing has proved to be one of the most difficult areas
; of Artificial Intelligence and Computer Science as a whole.  Translation is a complex task that not
; only involves understanding the semantic and grammar rules of a language, but also knowledge of the
; writers intent.  Additionally, multiple words in a language may serve different functions.  For example,
; as demonstrated in the examples "Buffalo" may refer to the city in New York, the animal (bison), or
; the verb that means "to bully". The classic and famous example is "Buffalo buffalo Buffalo buffalo buffalo buffalo Buffalo buffalo",
; which is a valid English sentence.
;
; My translator parses the sentence using the basic grammar S-V, S-V-DO, or S-V-IO (S = subject, V = verb, DO = direct object, 
; IO = indirect object).  Once the part-of-speech for a word has been determined it is easier to choose which french word
; or phrase to replace the english word with.
;
; Significant problems with this approach: 
;     +Only handle certain sentence structures
;     +Mine does not handle gender
;     +Difficult to re-order words correctly
;     +Mine only handles singular nouns
;     +Does not combine words (contractions that in English are optional, that are not in French) such as "à + le" = "au", etc
;     +Some idiocincracies related to racket especially with apostrophes. 
;
; The examples show some sentences from each category of the grammar and include words that can function in more than
; one way.


#lang racket

(require racket/trace)

(define articles '(the a))
(define adjectives '(black white Buffalo small big scary tall pretty))
(define conjunctions '(and))
(define nouns '(buffalo block bird cat dog hill))
(define prepositions '(over to up))
(define verbs '(chases block flies runs buffalo))

(define DB '( 
             ((adjective big) gros)
             ((adjective Buffalo) de Buffalo)
             ((adjective scary) effrayant)
             ((adjective pretty) mignon)
             ((adjective tall) grand)
             ((adjective black) noir)
             ((adjective white) blanc)
             ((adjective small) petit)
             ((article the) le)
             ((article a) un)
             ((conjunction and) et)
             ((noun bird) oiseau)
             ((noun buffalo) bison)
             ((noun block) bloc)
             ((noun cat) chat)
             ((noun dog) chien)
             ((noun hill) colline)
             ((preposition up) jusque à)
             ((preposition to) à)
             ((verb buffalo) fait peur)
             ((verb chases) poursuit)
             ((verb block) bloque) 
             ((verb runs) court) 
             ((verb flies) vole)
             ))

(define (conjunction? word) (if (member word conjunctions) #t #f))
(define (article? word) (if (member word articles) #t #f))
(define (noun? word) (if (member word nouns) #t #f))
(define (adjective? word) (if (member word adjectives) #t #f))
(define (verb? word) (if (member word verbs) #t #f))
(define (preposition? word) (if (member word prepositions) #t #f))

(define (mark-up sent)
  (printf "Translating: ~a.\n" sent)
  (let* ([first-word (car sent)] [rest-of-sentence (cdr sent)] [noun-phrase+verb-phrase (noun-phrase+verb-phrase? rest-of-sentence)])
  (cond
    ((article? first-word) (list (list 'article first-word) 
                                  (append (filter     (λ(word) (eq? (car word) 'noun)) noun-phrase+verb-phrase)
                                          (filter-not (λ(word) (eq? (car word) 'noun)) noun-phrase+verb-phrase)) 
                                  ))
    (noun-phrase+verb-phrase)
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
      ((noun? word) (list (list 'noun word)))
      ((not (pair? clause)) #f)
      ((conjunction? word) (list (adj+obj? (cdr clause))))
      ((adjective? word) (filter-not (λ(x) (null? x)) (append (adj+obj? (cdr clause)) (list (list 'adjective word)) (list (list 'conjunction 'and)))))
      (else #f)
    )))

(define (object-phrase clause)
  (let ([word (car clause)])
    (cond
      ((null? clause) #f)
      ((not (pair? clause)) #f)
      ((article? word) (list (list 'article word) (drop-right (adj+obj? (cdr clause)) 1)))
      (else #f)
    )))

(define (simple-subst word)
  (let ([sub (and (pair? word) (assoc word DB))])
    (if sub (cdr sub) #f)))

(define (english-to-french sent)
  (let ([ssub (simple-subst sent)])
    (if ssub ssub
        (flatten (append-map
                  (λ(phrase)
                    (cond
                      [(simple-subst phrase)]
                      [(not (pair? phrase)) '()]
                      [(append-map (λ(sub-phrase) (english-to-french sub-phrase)) phrase)]
                      [else '()]
                      ))
                  sent)))))

(define (translate sent)
  (english-to-french (mark-up sent)))

(translate '(the Buffalo buffalo buffalo the big scary dog))
(translate '(the dog runs up the hill))
(translate '(the pretty bird flies))
(translate '(the big scary black dog chases the small pretty white cat))
    