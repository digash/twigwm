;;;; Run with: ciel --no-userinit tests/macos-window-history.lisp
(load (merge-pathnames "fixture.lisp" *load-truename*))
(in-package :twigwm-macos-apps)

(when (uiop:os-macosx-p)
  (framework "ApplicationServices")
  (let ((frontmost (symbol-function 'frontmost))
        (movable (symbol-function 'movable-window))
        (text (symbol-function 'ax-text))
        (raise (symbol-function 'raise-window))
        (*window-history* nil))
    (unwind-protect
         (with-cf (one (cf-string "first Emacs window"))
           (with-cf (same (cf-string "first Emacs window"))
             (with-cf (two (cf-string "second Emacs window"))
               (with-cf (three (cf-string "browser window"))
                 (let ((selected one) (pid 100) (standard t) (closed nil) (calls nil))
                   (setf (symbol-function 'frontmost) (lambda () pid)
                         (symbol-function 'movable-window)
                         (lambda (id remote)
                           (assert (= id pid)) (assert (null remote))
                           (when selected
                             (cffi:foreign-funcall "CFRetain" :pointer selected :pointer)))
                         (symbol-function 'ax-text)
                         (lambda (window name)
                           (assert (cf-equal window selected))
                           (assert (equal name "AXSubrole"))
                           (if standard "AXStandardWindow" "AXDialog"))
                         (symbol-function 'raise-window)
                         (lambda (window id)
                           (when (and closed (cf-equal window closed))
                             (error "Window closed"))
                           (assert (= id (if (cf-equal window three) 200 100)))
                           (push window calls)
                           (setf selected window pid id)))
                   (assert (record-front-window))
                   (assert (not (previous-window))) ; only one known window
                   (setf selected same)
                   (record-front-window)
                   (assert (= 1 (length *window-history*))) ; CFEqual, not pointer equality
                   (setf selected two)
                   (record-front-window)
                   (assert (previous-window))
                   (assert (cf-equal selected one)) ; same application, distinct window
                   (assert (previous-window))
                   (assert (cf-equal selected two)) ; repeated toggles
                   (setf selected three pid 200)
                   (record-front-window)
                   (setf closed two)
                   (assert (previous-window))
                   (assert (cf-equal selected one)) ; skip closed window, keep order
                   (assert (= 2 (length *window-history*)))
                   (assert (previous-window))
                   (assert (cf-equal selected three)) ; cross-application identity
                   (setf standard nil)
                   (assert (not (record-front-window)))
                   (assert (not (previous-window))) ; dialog does not pick an arbitrary window
                   (setf selected nil)
                   (assert (not (previous-window)))
                   ;; A skipped remote session is never recorded; from it,
                   ;; the most recent local window is raised.
                   (setf standard t selected one pid 100)
                   (record-front-window)
                   (setf (symbol-function 'frontmost)
                         (lambda () (values pid "com.microsoft.rdc.macos")))
                   (assert (eq :skipped (record-front-window '("com.microsoft.rdc.macos"))))
                   (setf selected three pid 200)
                   (assert (previous-window '("com.microsoft.rdc.macos")))
                   (assert (cf-equal selected one))
                   (reset-window-history)
                   (assert (null *window-history*)))))))
      (reset-window-history)
      (setf (symbol-function 'frontmost) frontmost
            (symbol-function 'movable-window) movable
            (symbol-function 'ax-text) text
            (symbol-function 'raise-window) raise)))
  (format t "PASS: individual-window MRU, same-app toggling, CF identity, closed-window removal, dialogs, skipped remote sessions, and reset.~%"))
(format t "WINDOW_HISTORY_TESTS_COMPLETE~%")
