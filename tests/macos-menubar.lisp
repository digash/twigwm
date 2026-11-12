;;;; Run with: ciel --no-userinit tests/macos-menubar.lisp
(load (merge-pathnames "fixture.lisp" *load-truename*))
(in-package :twigwm-macos-input)

(assert (equal '((55 12 #x100008) (56 12 #x12000a) (23 10 #x12000a) (23 11 #x12000a)
                 (56 12 #x100008) (55 12 0))
               (key-chord 23 #x120000)))
(assert (equal '((58 12 #x80020) (109 10 #x80020) (109 11 #x80020) (58 12 0))
               (key-chord 109 #x80000)))
(assert (equal '((105 10 0) (105 11 0)) (key-chord 105 0)))
(format t "PASS: menu key chords press modifiers first and release them last, like a keyboard.~%")

(let ((equivalent (symbol-function 'twigwm-macos-menubar:key-equivalent))
      (posted nil))
  (unwind-protect
       (progn
         (setf (symbol-function 'twigwm-macos-menubar:key-equivalent)
               (lambda (code) (format nil "k~d" code)))
         (let* ((items (menu-items nil))
                (titled (remove :separator items))
                (titles (mapcar (lambda (item) (getf item :title)) titled)))
           (assert (equal '("Terminal (desktop)" "Browser (desktop)" "Show secondary"
                            "Move Window Left" "Move Window Right" "Move Window Up"
                            "Move Window Down" "Previous Window"
                            "Maximize Window" "Screenshot Region" "Art"
                            "Save Desktop" "Load Desktop" "Quit TwigWM")
                          titles))
           (assert (equal '("k18" #x100000) (let ((item (first titled)))
                                              (list (getf item :key) (getf item :modifiers)))))
           (assert (equal '("k109" #x80000)
                          (let ((item (find "Maximize Window" titled :key #'second :test #'equal)))
                            (list (getf item :key) (getf item :modifiers)))))
           (assert (null (getf (find "Save Desktop" titled :key #'second :test #'equal) :key)))
           (let ((post (symbol-function 'post-key-chord)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'post-key-chord)
                          (lambda (pid chord &optional tag) (push (list pid chord tag) posted)))
                    (funcall (getf (find "Move Window Left" titled :key #'second :test #'equal)
                                   :action)))
               (setf (symbol-function 'post-key-chord) post)))
           (assert (equal `((nil ,(key-chord 123 #x100000) 0)) posted))
           (let ((*quit* nil))
             (funcall (getf (car (last titled)) :action))
             (assert *quit*)))
         (let ((titles (mapcar (lambda (item) (getf item :title)) (remove :separator (menu-items t)))))
           (assert (equal '("Reload TwigWM" "Quit TwigWM") (last titles 2))))
         (let ((*number-device* nil) (*zero-device* nil))
           (assert (equal '("Terminal" "Browser" "Remote")
                          (mapcar (lambda (item) (getf item :title)) (subseq (menu-items nil) 0 3))))))
    (setf (symbol-function 'twigwm-macos-menubar:key-equivalent) equivalent)))
(format t "PASS: menu lists every shortcut with its key, types its chord untagged, and quits or reloads the agent.~%")

(assert (= 16 (length (twigwm-macos-menubar:clock "%y%b%d%a%H%M%S"))))
(assert (equal (twigwm-macos-menubar:clock "%Y")
               (princ-to-string (nth-value 5 (decode-universal-time (get-universal-time))))))
(format t "PASS: menu-bar clock is local strftime.~%")
