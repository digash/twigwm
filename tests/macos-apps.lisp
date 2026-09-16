;;;; Run with: ciel --no-userinit tests/macos-apps.lisp
(load (merge-pathnames "fixture.lisp" *load-truename*))
(in-package :twigwm-macos-apps)

;; Polling and shortcut workers share framework loads. Each name must register
;; once, even when first requested concurrently; later polling must be read-only.
(let ((loader (symbol-function 'cffi:load-foreign-library))
      (cache (make-hash-table :test #'equal))
      (calls 0))
  (unwind-protect
       (progn
         (setf (symbol-function 'cffi:load-foreign-library)
               (lambda (path)
                 (incf calls)
                 (sleep 0.001)
                 path))
         (let ((threads
                 (loop repeat 4 collect
                   (sb-thread:make-thread
                    (lambda ()
                      (let ((*frameworks* cache))
                        (dotimes (i 100)
                          (dolist (name '("AppKit" "ApplicationServices" "CoreFoundation"))
                            (assert (string= (framework name)
                                             (format nil "/System/Library/Frameworks/~a.framework/~a"
                                                     name name)))))))))))
           (dolist (thread threads) (sb-thread:join-thread thread)))
         (assert (= calls 3)))
    (setf (symbol-function 'cffi:load-foreign-library) loader)))
(format t "PASS: concurrent framework requests load each library once; polling adds no registrations.~%")

(defparameter *helper-source*
  (uiop:read-file-string
   (merge-pathnames "../src/macos-apps.lisp"
                    (uiop:pathname-directory-pathname *load-truename*))))
(dolist (forbidden '("osascript" "osacompile" "desktoprep" "uuidgen" "/bin/bash" "/bin/sh"))
  (assert (not (search forbidden *helper-source*))))
(dolist (required '("runningApplicationsWithBundleIdentifier:" "AXMenuBar" "AXPress" "AXFocusedWindow"))
  (assert (search required *helper-source*)))

(assert (eq :one (only '(:one) "test item")))
(dolist (items '(nil (:one :two)))
  (assert (handler-case (progn (only items "test item") nil) (error () t))))
(assert (handler-case (progn (with-cf (missing (cffi:null-pointer)) :unreachable) nil)
          (error (e) (search "returned no" (princ-to-string e)))))

;; desktop must select its named localhost session even when secondary is listed first.
(let ((open (symbol-function 'open-bundle))
      (focus (symbol-function 'focus)))
  (unwind-protect
       (progn
         (setf (symbol-function 'open-bundle)
               (lambda (bundle) (assert (equal bundle "com.microsoft.rdc.macos")))
               (symbol-function 'focus)
               (lambda (bundle title)
                 (assert (equal bundle "com.microsoft.rdc.macos"))
                 title))
         (assert (equal (focus-windows "desktop") "desktop"))
         (assert (null (focus-windows))))
    (setf (symbol-function 'open-bundle) open
          (symbol-function 'focus) focus)))

;; Exercise the actual native generator on macOS, with child processes forbidden.
(when (uiop:os-macosx-p)
  (let ((launch (symbol-function 'uiop:launch-program)))
    (unwind-protect
         (progn
           (setf (symbol-function 'uiop:launch-program)
                 (lambda (&rest args) (error "UUID generation spawned a child: ~s" args)))
           (let ((ids (loop repeat 64 collect (nonce))))
             (assert (= 64 (length (remove-duplicates ids :test #'string=))))
             (dolist (id ids)
               (assert (= 36 (length id)))
               (assert (loop for char across id for i from 0
                             always (if (member i '(8 13 18 23))
                                        (char= char #\-)
                                        (digit-char-p char 16)))))))
      (setf (symbol-function 'uiop:launch-program) launch)))
  (format t "PASS: 64 native UUIDs; unique, valid, and no child processes.~%"))

;; Mock only the OS process boundary; exercise stream capture and supervision.
(let ((launch (symbol-function 'uiop:launch-program))
      (alive (symbol-function 'uiop:process-alive-p))
      (wait (symbol-function 'uiop:wait-process))
      (terminate (symbol-function 'uiop:terminate-process))
      (argv '("/test/executable" "spaces ' quotes \" $values; literal" ""))
      (running nil)
      (killed nil)
      (waited nil)
      (launch-count 0))
  (unwind-protect
       (progn
         (setf (symbol-function 'uiop:launch-program)
               (lambda (command &key input output error-output)
                 (assert (equal command argv))
                 (assert (null input))
                 (incf launch-count)
                 (write-string "stdout" output)
                 (write-string "stderr" error-output)
                 (finish-output output)
                 (finish-output error-output)
                 :process)
               (symbol-function 'uiop:process-alive-p)
               (lambda (process) (assert (eq process :process)) running)
               (symbol-function 'uiop:wait-process)
               (lambda (process) (assert (eq process :process)) (setf waited t) 17)
               (symbol-function 'uiop:terminate-process)
               (lambda (process &key urgent)
                 (assert (eq process :process)) (assert urgent)
                 (setf running nil killed t)))
         (assert (equal (multiple-value-list (call argv)) '("stdout" "stderr" 17)))
         (assert waited)
         (assert (not killed))
         (setf running t waited nil)
         (let ((*command-timeout* 0))
           (assert (handler-case (progn (call argv) nil)
                     (error (e) (search "timed out" (princ-to-string e))))))
         (assert (and killed waited (= launch-count 2))))
    (setf (symbol-function 'uiop:launch-program) launch
          (symbol-function 'uiop:process-alive-p) alive
          (symbol-function 'uiop:wait-process) wait
          (symbol-function 'uiop:terminate-process) terminate)))

;; Exercise the resident's guest submission directly, including argv and no retry.
(let ((original-call (symbol-function 'call))
      (form "(print \"spaces ' quotes $values; literal\")")
      (ack "test-ack")
      (calls 0))
  (unwind-protect
       (dolist (case `((,(format nil "test-ack~%") "" 0 t)
                      (,(format nil "before~%test-ack~%after~%") "" 0 nil)
                      ("" "" 0 nil) ; SSH completion must include the acknowledgement.
                      ("guest agent is not running" "" 0 nil)
                      ("\"test-ack\"" "" 0 nil)
                      ("old-ack" "" 0 nil)
                      ("error evaluating (test-ack)" "" 0 nil)
                      ("test-ack:unexpected" "" 0 nil)
                      ("" "transport failed" 17 nil)
                      ("test-ack" "transport failed" 17 nil)
                      ("" "test-ack" 0 nil)
                      (:spawn-error "" 0 nil)))
         (destructuring-bind (out err status success) case
           (setf calls 0
                 (symbol-function 'call)
                 (lambda (argv)
                   (incf calls)
                   (assert
                    (equal argv
                           (append *guest-command* (list form ack))))
                   (when (eq out :spawn-error) (error "Cannot spawn vm"))
                   (values out err status)))
           (assert (eql success
                        (handler-case (progn (submit form ack) t)
                          (error () nil))))
           (assert (= calls 1))))
    (setf (symbol-function 'call) original-call)))

(format t "PASS: guest submission preserves argv, validates replies and never retries.~%")
(format t "PASS: process adapter preserves results and kills timed-out children.~%")
