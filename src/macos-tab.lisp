;;;; Stateful Tab translation; event descriptions are (KEYCODE TYPE FLAGS).
(in-package :twigwm-macos-input)

(defvar *swap-tab-modifiers* nil
  "Option-Tab switches local apps; Command-Tab sends Alt-Tab to remote apps.")
(defstruct tab-switch source pid tab-down (flags 0))
(defvar *tab-switch* nil)

(defun translated-tab-flags (switch flags)
  "Replace the held source modifier with the left destination modifier."
  (let* ((option-p (eq (tab-switch-source switch) :option))
         (source (if option-p #x80000 #x100000))
         (destination (if option-p #x100008 #x80020)))
    (logior (logandc2 flags #x180078)
            (if (logtest flags source) destination 0))))

(defun tab-switch-step (code type flags &optional remote-p pid)
  "Return consumed-p, destination PID (NIL is global), and translated events.
Pin the destination until release, including a Tab-up after modifier-up.
Only bare/Shift Tab starts a gesture; unrelated shortcuts pass through."
  (let* ((modifiers (logand flags #x1e0000))
         (start (and (null *tab-switch*) (= code 48) (= type 10)
                     (cond ((member modifiers '(#x80000 #xa0000)) :option)
                           ((and remote-p (member modifiers '(#x100000 #x120000)))
                            :command)))))
    (when start
      (setf *tab-switch* (make-tab-switch :source start
                                         :pid (and (eq start :command) pid))))
    (when *tab-switch*
      (let* ((switch *tab-switch*)
             (option-p (eq (tab-switch-source switch) :option))
             (source (if option-p #x80000 #x100000))
             (source-codes (if option-p '(58 61) '(55 54)))
             (source-bits (if option-p '(#x20 #x40) '(#x8 #x10)))
             (destination (if option-p 55 58))
             (translated (translated-tab-flags switch flags)))
        ;; After modifier-up, only the outstanding Tab-up belongs to this gesture.
        (unless (or start (logtest (tab-switch-flags switch) source)
                    (and (= code 48) (= type 11)))
          (return-from tab-switch-step nil))
        (let ((events nil))
          (when start
            ;; The app already saw the physical modifier-down. Release it before
            ;; introducing its replacement, so the guest never sees Super+Alt.
            (loop for key in source-codes for bit in source-bits
                  when (or (logtest flags bit)
                           (and (= key (first source-codes))
                                (not (logtest flags (reduce #'logior source-bits)))))
                    do (push (list key 12 (logandc2 flags #x180078)) events))
            (push (list destination 12 translated) events))
          (cond
            ((and (= type 12) (member code source-codes))
             ;; Two physical sides can be held: finish only after both are up.
             (unless (logtest flags source)
               (push (list destination 12 translated) events)))
            (t (push (list code type translated) events)))
          (when (= code 48)
            (setf (tab-switch-tab-down switch) (= type 10)))
          (setf (tab-switch-flags switch) flags)
          (unless (or (logtest flags source) (tab-switch-tab-down switch))
            (setf *tab-switch* nil))
          (values t (tab-switch-pid switch) (nreverse events)))))))
