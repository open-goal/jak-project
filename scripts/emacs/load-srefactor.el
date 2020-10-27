;; Run with > emacs -batch -l ./scripts/emacs/load-srefactor.el <total processes> <process index>

;; Load srefactor
;; NOTE - we have modified it in a few ways to better support the GOAL LISP dialect
(add-to-list 'load-path "./third-party/srefactor")
(require 'srefactor)
(require 'srefactor-lisp)
;; Load benchmark.el for benchmarking
(add-to-list 'load-path "./third-party/benchmark")
(require 'benchmark)

(defvar total-processes (string-to-number (nth 3 command-line-args)))
(defvar process-index (string-to-number (nth 4 command-line-args)))

;; Load all *.gs, *.gd, and *.gc files
(defun load-goal-files ()
  (directory-files-recursively "." ".*\\.g[cds]$"))

(defun batch-files (files batch-size batch-index)
  (nth batch-index
       (seq-partition files
		      (/ (length files)
			 batch-size))))

(defvar goal-files (load-goal-files))

;; ---- Test
;; (pp (batch-files goal-files total-processes process-index))

(defun format-file (file)
  ;; Insert file into current buffer
  (find-file file)
  ;; Format with srefactor
  (srefactor-lisp-format-buffer)
  ;; Save current buffer to file
  (save-buffer)
  (kill-buffer)
  ;; (message (format "Formatted - %s" file)
  )

;; ---- TEST
;; (defvar test "./scripts/emacs/load-srefactor.el")
;; (pp (format "Formatting Took - %.2f seconds"
;; 	    (benchmark-elapse (format-file test))))
;; ----

;; TODO - make this faster by working through the files in batches in multiple emacs processes
;; srefactor only works on the current buffer, so it's pointless to open multiple files at a time in a single process
;; Iterate through all of them!

(defvar files-to-format (batch-files goal-files total-processes process-index))

(message (format "[EMACS-%d] Formatting %d goal files (*.gs, *.gc, *.gd)"
		 process-index
		 (length files-to-format)))

(pp (format "[EMACS-%d] Formatting Took - %.2f seconds"
	    process-index
	    (benchmark-elapse (mapc 'format-file files-to-format))))

