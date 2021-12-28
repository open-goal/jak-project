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

(defsmacro unless (clause &rest body)
  `(if (not ,clause) (begin ,@body) #f)
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

(defsmacro caddr (x)
  `(car (cdr (cdr ,x))))

(defsmacro cdddr (x)
           `(cdr (cdr (cdr ,x))))

(defsmacro cadddr (x)
           `(car (cdr (cdr (cdr ,x)))))

(defsmacro caadr (x)
  `(car (car (cdr ,x))))

(defsmacro cadar (x)
  `(car (cdr (car ,x))))

(desfun first (x)
	(car x))

(desfun rest (x)
  (cdr x))

(desfun second (x)
	(car (cdr x))
	)

(desfun third (x)
        (car (cddr x)))

(defsmacro push! (lst x)
  `(set! ,lst (cons ,x ,lst))
  )
(defsmacro pop! (lst)
  `(set! ,lst (cdr ,lst))
  )

(desfun apply (fun x)
	(if (null? x)
	    '()
	    (cons (fun (car x))
		        (apply fun (cdr x))
		        )
	    )
	)

;; same as apply but interleaves with two functions and lists
(desfun apply2 (fun1 fun2 lst1 lst2)
	(if (or (null? lst1) (null? lst2) (not (= (length lst1) (length lst2))))
	    '()
	    (cons (fun1 (car lst1))
		        (cons (fun2 (car lst2))
                  (apply2 fun1 fun2 (cdr lst1) (cdr lst2))
                  )
		        )
	    )
	)

(desfun filter (pred lst)
  (cond ((null? lst) '())
        ((pred (first lst))
         (cons (first lst) (filter pred (cdr lst))))
        (#t (filter pred (cdr lst)))))

(desfun assoc (x a)
  (if (null? a)
      '()
      (if (eq? (caar a) x)
          (car a)
          (assoc x (cdr a))
          )
      )
	)

(desfun list (&rest items)
  (apply (lambda (x) x) items)
  )

(desfun reverse (lst)
  (if (null? lst)
    '()
    (let ((old-lst lst)
          (new-lst '()))
      (while (not (null? old-lst))
        (set! new-lst (cons (car old-lst) new-lst))
        (set! old-lst (cdr old-lst))
        )
      new-lst
      )
    )
  )

(desfun reverse-recursive (lst)
  (if (null? lst)
    '()
    (let ((old-lst lst)
          (new-lst '()))
      (while (not (null? old-lst))
        (let ((cur-obj (car old-lst)))
          (set! new-lst (cons (if (pair? cur-obj)
                                  (reverse-recursive cur-obj)
                                  cur-obj
                                  )
                              new-lst))
          (set! old-lst (cdr old-lst))
          )
        )
      new-lst
      )
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

(defsmacro symbol? (x)
  `(type? 'symbol ,x)
  )

(defsmacro ferror (&rest args)
  `(error (fmt #f ,@args))
  )


(desfun apply-i-fun (fun x i)
  (if (null? x)
      '()
      (cons (fun (car x) i)
            (apply-i-fun fun (cdr x) (inc! i))
            )
      )
	)
(defsmacro apply-i (fun x)
  `(apply-i-fun ,fun ,x 0)
  )

(defsmacro string->symbol-format (str &rest args)
  `(string->symbol (fmt #f ,str ,@args))
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

;;;;;;;;;;;;;;;;;;;
;; enum stuff
;;;;;;;;;;;;;;;;;;;

(desfun enum-length (enum)
  (length (get-enum-vals enum))
  )

(defsmacro doenum (bindings &rest body)
  ;; (doenum (name-var val-var 'enum &rest result) &rest body)
  
  (with-gensyms (enum-vals)
    `(let ((,enum-vals (get-enum-vals ,(third bindings))))
        
        (while (not (null? ,enum-vals))
          (let ((,(first bindings) (caar ,enum-vals)) ;; name
                (,(second bindings) (cdar ,enum-vals)) ;; value
                )
            ,@body
            )
          
          (set! ,enum-vals (cdr ,enum-vals))
          )
        
        ,@(cdddr bindings)
        
        )
    )
  
  )

(desfun enum-max (enum)
  "get the highest value in an enum"
  
  (let ((max-val -999999999))
    (doenum (name val enum)
      (when (> val max-val)
        (set! max-val val))
      )

    max-val
    )
  )


;; shortcut to quit GOOS
(defsmacro e ()
  `(exit)
  )

;; this is checked in a test to see if this file is loaded.
(define __goos-lib-loaded__ #t)


;;;;;;;;;;;;;;;;;;;;;;;;
;; USER PROFILES      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; *user* is defined when goos starts!
(when *user*
  (fmt #t "Loading user scripts for user: {}...\n" *user*)
  ;; i'm not sure what naming scheme to use here. user/<name>/user.gs?
  ;; the GOAL one is loaded in Compiler.cpp
  (load-file (fmt #f "goal_src/user/{}/user.gs" *user*))
  )

(defsmacro user? (&rest users)
  (cond
    ((null? users)            #f)
    ((eq? *user* (car users)) #t)
    (#t   `(user? ,@(cdr users)))
    )
  )


