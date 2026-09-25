(load (merge-pathnames "../src/load.lisp" *load-truename*))
(in-package :twigwm-macos-input)
;; The public project has no private registry, display IDs, or remote target.
(assert (null twigwm-apps:*apps*))
(assert (null *number-device*))
(assert (null *prefix-number-device*))
(assert (equal twigwm-macos-apps:*displays* '((:main :primary 0))))
(assert (equal '(10 20 1200 800)
               (twigwm-macos-apps::region-rect :main
                  '(("synthetic" (0 0 1200 900) (10 20 1200 800))))))
;; Ordinary application shortcuts remain usable without remote routing.
(let ((field (symbol-function 'event-field))
      (bundle (symbol-function 'twigwm-apps:mac-bundle))
      (twigwm-apps:*apps* (list (twigwm-apps:make-app :name "editor" :n 1 :mac "org.gnu.Emacs")))
      (*app-actions* nil) (*app-down* (make-hash-table))
      (code 18))
  (unwind-protect
       (progn
         (setf (symbol-function 'event-field) (lambda (&rest args) (declare (ignore args)) code)
               (symbol-function 'twigwm-apps:mac-bundle) #'twigwm-apps:app-mac)
         (assert (dispatch-number :event))
         (assert (equal (caar *app-actions*) :activate))
         (assert (equal (third (first *app-actions*)) "org.gnu.Emacs"))
         (assert (gethash code *app-down*))
         (setf code 19)
         (assert (not (dispatch-number :event)))
         (assert (not (gethash code *app-down*))))
    (setf (symbol-function 'event-field) field
          (symbol-function 'twigwm-apps:mac-bundle) bundle)))
(format t "CONFIGURATION_TESTS_COMPLETE~%")
