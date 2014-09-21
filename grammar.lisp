(defparameter *grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adjective Noun)(Article Noun)(Pronoun))
    (Pronoun -> she he it)
    (Adjective -> nice blue soft warm sad)
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")


(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order
;(THE MAN LIKED A WOMAN)
(defparameter rules1 '((sentence 0) (noun-phrase 1) (Article 0) (Noun 0) (verb-phrase 0) (Verb 3) (noun-phrase 1) (Article 1) (Noun 2)))

;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)

(defun apply-rules (rules sentence)
  (cond 
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule)))))) 

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t 
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))
      

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun random-sentence (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'random-sentence phrase))
        ((rewrites phrase)
         (random-sentence (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))


(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generateValid (phrase)
	"Generates only good sentences"
	(setq p (random-sentence phrase))
	(setq a (list-length p))
	(loop while (< 5 a) do
		(with-open-file (outfile "q2_jcf2167.txt":direction :output
					:if-exists :append
					:if-does-not-exist :create)
			(print p outfile))
		(setq p (random-sentence phrase))
		(setq a (list-length p)))
	(print p)
	;(print phrase)
	;(if ((list-length (random-sentence 'sentence)))
	
	;(setq a 'sentence)
	;(print (array-total-size 'a))
)
(generateValid 'sentence)
(with-open-file (outfile "q1_jcf2167.txt":direction :output
                     :if-exists :supersede 
                     :if-does-not-exist :create)
    (loop for i from 1 to 10 do
    	(print (random-sentence 'sentence) outfile)
	)
)
