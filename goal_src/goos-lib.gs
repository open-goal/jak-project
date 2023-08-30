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

(defsmacro aif (condition true false)
  "Anaphoric if, similar to Common Lisp"

  `(let ((it ,condition))
      (if it
          ,true
          ,false
          )
      )
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

(desfun count (lst)
  (if (null? lst)
      0
      (+ 1 (count (cdr lst))))
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

(desfun member (x a)
  (if (null? a)
      #f
      (if (eq? (car a) x)
          a
          (member x (cdr a))
          )
      )
	)

(desfun assoc (x a)
  (if (null? a)
      #f
      (if (eq? (caar a) x)
          (car a)
          (assoc x (cdr a))
          )
      )
	)

(desfun assocn (x a)
  (let ((i -1)
        (ret -1)
        (iter a))
    (while (and (< ret 0) (not (null? iter)))
      (inc! i)
      (when (eq? (car iter) x)
          (set! ret i))
      (set! iter (cdr iter)))
    ret)
  )

(desfun nth (n a)
  (let ((i -1)
        (ret #f)
        (stop? #f)
        (iter a))
    (while (and (not stop?) (not (null? iter)))
      (inc! i)
      (when (eq? i n)
          (set! ret (car iter))
          (set! stop? #t))
      (set! iter (cdr iter)))
    ret)
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

(defsmacro dolist (bindings &rest body)
  `(let ((,(car bindings) ,(cadr bindings)))
      (while (not (null? ,(car bindings)))
        ,@body

        (set! ,(car bindings) (cdr ,(car bindings)))
        )
      )
  )

(defsmacro cons! (lst obj) `(set! ,lst (cons ,obj ,lst)))

(desfun append! (lst obj)
  "adds obj to the end of lst. only edits inplace if lst is not null."
  (let ((pended #f))
    (if (null? lst)
        (set! lst (cons obj '()))
        (dolist (it lst)
            (when (and (not pended) (null? (cdr it)))
              (set-cdr! it (cons obj '()))
              (set! pended #t)
              )
            )
        )
    )
  lst
  )

(desfun delete-last! (lst)
  "removes last item in lst.  only edits inplace if lst is not null."
  (let ((prev '()))
    (dolist (it lst)
        (when (null? (cdr it))
          (if (null? prev)
              (set! lst '())
              (set-cdr! prev '())
              )
          )
        (set! prev it)
        )
    )
  lst
  )

(defsmacro append!! (lst obj) `(set! ,lst (append! ,lst ,obj)))
(defsmacro delete-last!! (lst) `(set! ,lst (delete-last! ,lst)))


(defsmacro 1+ (x) `(+ ,x 1))
(defsmacro 1- (x) `(- ,x 1))
(defsmacro inc! (x) `(set! ,x (1+ ,x)))
(defsmacro dec! (x) `(set! ,x (1- ,x)))
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

(defsmacro number? (x)
  `(or (float? ,x) (integer? ,x))
  )

(defsmacro neq? (a b) `(not (eq? ,a ,b)))

(defsmacro != (a b) `(not (= ,a ,b)))
(defsmacro zero? (x) `(= ,x 0))
(defsmacro nonzero? (x) `(!= ,x 0))

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
     ,(if (and (> (length body) 1) (string? (first body)))
          ;; then it's a docstring and we ignore it.
          `(begin
            (update-macro-metadata ,name ,(first body) ,args)
            (seval (defgmacro ,name ,args ,@(cdr body))))
          ;; otherwise don't ignore it.
          `(begin
            (update-macro-metadata ,name "" ,args)
            (seval (defgmacro ,name ,args ,@body)))
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

  (let ((max-val -999999999999))
    (doenum (name val enum)
      (when (> val max-val)
        (set! max-val val))
      )
    max-val)
  )

(defgmacro enum-max (enum)
  (enum-max enum))
(defgmacro enum-length (enum)
  (enum-length enum))


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
  (try-load-file (fmt #f "goal_src/user/{}/user.gs" *user*))
  )

(defsmacro user? (&rest users)
  (cond
    ((null? users)            #f)
    ((eq? *user* (car users)) #t)
    (#t   `(user? ,@(cdr users)))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME STUFF!!!      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; a map for art definitions used by art loading code.
(define *art-info* (make-string-hash-table))


;;;;;;;;;;;;;;;;;;;;;;;;
;;  build system      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define GAME_TERRITORY_SCEA 0)
(define GAME_TERRITORY_SCEE 1)
(define GAME_TERRITORY_SCEI 2)
(define GAME_TERRITORY_SCEK 3)

(define *jak1-full-game* (if (user? dass) #t #f))
(define *default-territory* GAME_TERRITORY_SCEA)

;; whether to enable ps3 test levels for jak 2
(define USE_PS3_LEVELS #f)