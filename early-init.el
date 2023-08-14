;;;; Speed up startup
;; Help speed up emacs initialization See
;; https://blog.d46.us/advanced-emacs-startup/ and
;; http://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html and
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This will be set back to normal at the end of the init file

(defvar lem-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; Garbage collection

;; Defer garbage collection further back in the startup process. We'll lower
;; this to a more reasonable number at the end of the init process (i.e. at end of
;; init.el)

(setq gc-cons-threshold most-positive-fixnum)

;; Adjust garbage collection thresholds during startup, and thereafter
;; See http://akrl.sdf.org https://gitlab.com/koral/gcmh

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (let ((inhibit-message t))
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

;;;; Native Comp

;; See https://github.com/jimeh/build-emacs-for-macos#native-comp
;; https://akrl.sdf.org/gccemacs.html#org335c0de
;; https://github.com/emacscollective/no-littering/wiki/Setting-gccemacs'-eln-cache
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=53891
;; https://emacs.stackexchange.com/a/70478/11934

;; See if native-comp is available
(cond ((not (and (fboundp 'native-comp-available-p)
                 (native-comp-available-p)))
       (message "Native complation is *not* available"))
      ;; Put eln-cache dir in cache directory
      ;; NOTE the method for setting the eln-cache dir depends on the emacs version
      ((version< emacs-version "29")
       (setcar native-comp-eln-load-path
               (expand-file-name (convert-standard-filename "var/cache/eln-cache/") user-emacs-directory)))
      (t
       (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/cache/eln-cache/" user-emacs-directory)))))

;; Silence nativecomp warnings popping up
(customize-set-variable 'native-comp-async-report-warnings-errors nil)

;; Native-comp settings
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

;;;; Set C Directory
;; NOTE this assumes that the C source files are included with emacs.
;; This depends on the build process used.
;; For one example see https://github.com/mclear-tools/build-emacs-macos
(setq find-function-C-source-directory "/Applications/Emacs.app/Contents/Resources/src")

;;;; Prefer Newer files
;; Prefer newer versions of files
(setq load-prefer-newer t)

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
(customize-set-variable 'byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
;;;; Check Errors
;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(customize-set-variable 'debug-on-error nil)

;;;; When-let errors
;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

;;;; Variable Binding Depth
;; This variable controls the number of lisp bindings that can exists at a time.
;; We should make it fairly large for modern machines.
;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(customize-set-variable 'max-specpdl-size 13000)

;;;; Customize interface file
(setq custom-file (concat user-emacs-directory "persistent/custom.el"))
(load-file custom-file)

;;;; Seaman core
(add-to-list 'load-path (concat user-emacs-directory "elisp/core/"))

(defvar shapes-core-components '("load"
                                 "package-manager"
                                 "config-management"
                                 "base-config"))

(mapc (lambda (component) (require (intern (concat "shapes-core-" component)) (concat "shapes-" component)))
      shapes-core-components)

(setq user-emacs-directory (expand-file-name user-emacs-directory))

(provide 'shapes-io/.-)
;;; shapes-.el ends here
