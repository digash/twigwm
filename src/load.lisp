;;;; Copyright (c) 2026 TwigWM contributors. MIT License.
(load (merge-pathnames "macos-input.lisp" *load-truename*))
(defpackage :twigwm
  (:use :cl)
  (:export #:main #:load-config #:*session-function*))
(in-package :twigwm)

(defvar *source-directory* (uiop:pathname-directory-pathname *load-truename*))
(defvar *session-function* nil)

(defun load-config (&optional
                      (path (or (uiop:getenv "TWIGWM_CONFIG")
                                (merge-pathnames "twigwm/config.lisp"
                                  (uiop:ensure-directory-pathname
                                    (or (uiop:getenv "XDG_CONFIG_HOME")
                                        (merge-pathnames ".config/" (user-homedir-pathname))))))))
  "Load optional local Lisp configuration. Never load it in tests implicitly."
  (when (probe-file path)
    (let ((*package* (find-package :cl-user))) (load path))))

(defun main (args)
  (load-config)
  (let ((command (first args)))
    (cond
      ((equal command "mac-input") (twigwm-macos-input:run))
      ((equal command "mac-input-trial") (twigwm-macos-input:run :seconds 120 :modifiers #x140000))
      ((equal command "mac-input-poc") (twigwm-macos-input:probe))
      ((equal command "save-desktop") (twigwm-macos-apps:save-desktop))
      ((equal command "load-desktop") (twigwm-macos-apps:load-desktop))
      ((equal command "session")
       (if *session-function* (funcall *session-function*)
           (uiop:run-program '("stumpwm") :output :interactive :error-output :interactive)))
      ((member command '(nil "--help" "help") :test #'equal)
       (format t "TwigWM: mac-input | mac-input-trial | mac-input-poc | save-desktop | load-desktop | session~%"))
      (t (error "Unknown TwigWM command: ~a" command)))))
