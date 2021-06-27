;-*-Scheme-*-

;; THE GOOS COMMON LIBRARY

;; goos macro to define a new goos macro
(define defsmacro (macro (name args &rest body)
		    `(define ,name (macro ,args ,@body))
		    )
  )

;; macro to define a new goos function
(defsmacro desfun (name args &rest body)
  `(define ,name (lambda ,args ,@body))
  )

;; goos macro to let us define goal macros from goos:
(defsmacro defgmacro (name args &rest body)
  `(define :env *goal-env* ,name (macro ,args ,@body))
  )


(defsmacro if (clause true false)
  `(cond (,clause ,true) (#t ,false))
  )

(defsmacro when (clause &rest body)
  `(if ,clause (begin ,@body) #f)
  )

(defsmacro not (x)
  `(if ,x #f #t)
  )

(desfun factorial (x)
	(if (= x 1)
	    1
	    (* x (factorial (- x 1)))
	    )
	)

(defsmacro max (a b)
  `(if (> a b) a b)
  )

(defsmacro min (a b)
  `(if (< a b) a b)
  )

(defsmacro caar (x)
  `(car (car ,x)))

(defsmacro cadr (x)
  `(car (cdr ,x)))

(defsmacro cdar (x)
  `(cdr (car ,x)))

(defsmacro cddr (x)
           `(cdr (cdr ,x)))

(desfun first (x)
	(car x))

(desfun rest (x)
  (cdr x))

(desfun second (x)
	(car (cdr x))
	)

(desfun third (x)
        (car (cddr x)))

(desfun apply (fun x)
	(if (null? x)
	    '()
	    (cons (fun (car x))
		        (apply fun (cdr x))
		        )
	    )
	)

(desfun filter (pred lst)
  (cond ((null? lst) '())
        ((pred (first lst))
         (cons (first lst) (filter pred (cdr lst))))
        (#t (filter pred (cdr lst)))))

(desfun assoc (x a)
	(if (eq? (caar a) x)
	    (car a)
	    (assoc x (cdr a))
	    )
	)

(defsmacro let (bindings &rest body)
  `((lambda ,(apply first bindings) ,@body)
    ,@(apply second bindings)))

(defsmacro let* (bindings &rest body)
  (if (null? bindings)
      `(begin ,@body)
      `((lambda (,(caar bindings))
	  (let* ,(cdr bindings) ,@body))
	;;(begin ,@(cdar bindings))
	,(car (cdar bindings))
	)
      )
  )

(defsmacro dotimes (var &rest body)
  `(let (( ,(first var) 0))
     (while (< ,(first var) ,(second var))
            ,@body
            (set! ,(first var) (+ ,(first var) 1))
            )
     ,@(cddr var)
     )
  )

(desfun repeated-list (obj count)
  (if (= 0 count)
    '()
    (cons obj (repeated-list obj (- count 1)))
    )
  )

(defsmacro with-gensyms (names &rest body)
  `(let
     ,(apply (lambda (x) `(,x (gensym))) names)
     ,@body
     )
  )


(desfun length (lst)
	(if (null? lst)
	    0
	    (+ 1 (length (cdr lst)))
	    )
	)

(defsmacro inc! (x) `(set! ,x (+ ,x 1)))
(defsmacro dec! (x) `(set! ,x (- ,x 1)))
(defsmacro +! (place arg) `(set! ,place (+ ,place ,arg)))
(defsmacro -! (place arg) `(set! ,place (- ,place ,arg)))


(defsmacro string? (x)
  `(type? 'string ,x))

(defsmacro float? (x)
  `(type? 'float ,x)
  )

(defsmacro integer? (x)
  `(type? 'integer ,x)
  )

(defsmacro pair? (x)
  `(type? 'pair ,x)
  )

(defsmacro ferror (&rest args)
  `(error (fmt #f ,@args))
  )


;; Bootstrap GOAL macro system


;; goal macro to define a goal macro
(defgmacro defmacro (name args &rest body)
  `(begin
     (add-macro-to-autocomplete ,name)
     ,(if (and
           (> (length body) 1) ;; more than one thing in function
           (string? (first body)) ;; first thing is a string
           )
          ;; then it's a docstring and we ignore it.
          `(seval (defgmacro ,name ,args ,@(cdr body)))
          ;; otherwise don't ignore it.
          `(seval (defgmacro ,name ,args ,@body))
          )
     )
  )

;; goal macro to define a goos macro
(defgmacro defsmacro (name args &rest body)
  `(seval (defsmacro ,name ,args ,@body))
  )

;; goal macro to define a goos function
(defgmacro desfun (name args &rest body)
  `(seval (desfun ,name ,args ,@body))
  )




;; shortcut to quit GOOS
(defsmacro e ()
  `(exit)
  )

;; this is checked in a test to see if this file is loaded.
(define __goos-lib-loaded__ #t)
