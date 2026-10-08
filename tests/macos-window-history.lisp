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
                  ;; One press and release of Command-Escape.
                  (flet ((press (&optional remote)
                           (prog1 (previous-window remote) (finish-cycle))))
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
                   (assert (not (press))) ; only one known window
                   (setf selected same)
                   (record-front-window)
                   (assert (= 1 (length *window-history*))) ; CFEqual, not pointer equality
                   (setf selected two)
                   (record-front-window)
                   (assert (press))
                   (assert (cf-equal selected one)) ; same application, distinct window
                   (assert (press))
                   (assert (cf-equal selected two)) ; repeated toggles
                   (setf selected three pid 200)
                   (record-front-window)
                   (setf closed two)
                   (assert (press))
                   (assert (cf-equal selected one)) ; skip closed window, keep order
                   (assert (= 2 (length *window-history*)))
                   (assert (press))
                   (assert (cf-equal selected three)) ; cross-application identity
                   (setf standard nil)
                   (assert (not (record-front-window)))
                   (assert (not (press))) ; dialog does not pick an arbitrary window
                   (setf selected nil)
                   (assert (not (press)))
                   ;; A remote session's window is recorded like any other,
                   ;; so toggling returns to that exact window.
                   (setf standard t selected one pid 100)
                   (record-front-window)
                   (setf (symbol-function 'frontmost)
                         (lambda () (values pid "com.microsoft.rdc.macos")))
                   (setf selected three pid 200)
                   (assert (record-front-window))
                   (assert (press '("com.microsoft.rdc.macos")))
                   (assert (cf-equal selected one))
                   (assert (press '("com.microsoft.rdc.macos")))
                   (assert (cf-equal selected three))
                   ;; Held Command cycles deeper, like s-Tab; the order is frozen
                   ;; until release, which makes the choice most recent.
                   (setf selected two pid 100 closed nil)
                   (record-front-window)          ; MRU: two three one
                   (assert (previous-window))
                   (assert (cf-equal selected three))
                   (assert (not (record-front-window))) ; watcher frozen mid-cycle
                   (assert (previous-window))
                   (assert (cf-equal selected one))
                   (assert (previous-window))
                   (assert (cf-equal selected two)) ; wraps around
                   (assert (previous-window))
                   (assert (cf-equal selected three))
                   (finish-cycle)
                   (assert (cf-equal (car (first *window-history*)) three))
                   (assert (cf-equal (car (second *window-history*)) two))
                   (assert (press))
                   (assert (cf-equal selected two)) ; a single press still toggles
                   (reset-window-history)
                   (assert (null *window-history*))))))))
      (reset-window-history)
      (setf (symbol-function 'frontmost) frontmost
            (symbol-function 'movable-window) movable
            (symbol-function 'ax-text) text
            (symbol-function 'raise-window) raise)))
  (format t "PASS: individual-window MRU, same-app toggling, CF identity, closed-window removal, dialogs, remote windows, held-modifier cycling, and reset.~%"))
(format t "WINDOW_HISTORY_TESTS_COMPLETE~%")
