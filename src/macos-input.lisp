;;;; Native input tap: isolated probes and resident app shortcuts.
(load (merge-pathnames "macos-apps.lisp" *load-truename*))
(load (merge-pathnames "apps.lisp" *load-truename*))
(load (merge-pathnames "keys.lisp" *load-truename*))
(defpackage :twigwm-macos-input
  (:use :cl)
  (:export #:probe #:run #:*number-device* #:*zero-device* #:*swap-tab-modifiers*))
(in-package :twigwm-macos-input)
(load (merge-pathnames "macos-tab.lisp" *load-truename*))

;;; Pure FIFO: readiness is explicit and belongs to one handoff generation.
(defstruct handoff
  (generation 0) pending events (count 0) (limit 128))

(defun arm (handoff)
  (unless (handoff-pending handoff)
    (setf (handoff-pending handoff) t)
    (incf (handoff-generation handoff)))
  (handoff-generation handoff))

(defun enqueue (handoff event)
  (assert (handoff-pending handoff))
  (when (>= (handoff-count handoff) (handoff-limit handoff))
    (error "Keyboard handoff buffer full"))
  (push event (handoff-events handoff))
  (incf (handoff-count handoff)))

(defun finish (handoff generation)
  "Return the FIFO and true only for the pending generation; otherwise no-op."
  (when (and (handoff-pending handoff)
             (= generation (handoff-generation handoff)))
    (multiple-value-prog1 (values (nreverse (handoff-events handoff)) t)
      (setf (handoff-pending handoff) nil
            (handoff-events handoff) nil
            (handoff-count handoff) 0))))

;;; CoreGraphics callback: probes capture tagged events; live mode routes app shortcuts.
;;; No subprocesses, waits, guest calls, or real keystroke logging here.
(defconstant +test-tag+ #x4349454c)
(defconstant +replay-tag+ #x4349454d)
(defconstant +heartbeat-tag+ #x4349454e)
(defconstant +tab-tag+ #x4349454f)
(defconstant +command+ #x100000)
(defvar *handoff*)
(defvar *callback-error* nil)
(defvar *replayed* nil)
(defvar *heartbeat* nil)
(defvar *probe-seen* nil)
(defvar *live* nil)
(defvar *prefix-modifiers* +command+)
(defvar *escape-down* nil)
(defvar *host-prefix-p* nil)
(defvar *lease* nil)
(defvar *number-device* nil)
(defvar *zero-device* nil)
(defvar *remote-bundles* nil)
(defvar *app-down* (make-hash-table))
(defvar *app-actions* nil)
(defvar *app-thread* nil)


(defun queue-app-action (&rest action)
  (setf *app-actions* (nconc *app-actions* (list action))))

(defun movement-direction (code modifiers)
  (when (= modifiers +command+)
    (cdr (assoc code '((123 . :left) (124 . :right) (125 . :down) (126 . :up))))))

(defun queue-window-move (pid bundle direction)
  (let ((window (twigwm-macos-apps:movable-window
                 pid (member bundle *remote-bundles* :test #'equal))))
    (when window (queue-app-action :move window direction) t)))

(defun release-app-action (action)
  (when (eq (first action) :move)
    (cffi:foreign-funcall "CFRelease" :pointer (second action) :void)))

(defun number-key (code)
  (position code #(29 18 19 20 21 23 22 26 28 25)))

(defun number-p (code modifiers)
  (and (number-key code) (/= code 29) (= modifiers +command+)))

(defun native-key-spec (code)
  "Find the native binding for a macOS virtual keycode."
  (loop for key in twigwm-keys:*keys* for mac = (twigwm-keys:key-mac key)
        when (eql code (getf mac :keycode)) return mac))

(defun native-key-action (spec modifiers bundle)
  (when (= modifiers (getf spec :modifiers 0))
    (or (cdr (assoc bundle (getf spec :chord) :test #'equal))
        (unless (member bundle *remote-bundles* :test #'equal)
          (or (getf spec :bundle)
              (when (getf spec :argv) (list :launch (getf spec :argv))))))))

(defun launch-native-command (argv)
  ;; Do not wait for the viewer to close: app shortcuts must stay responsive.
  (uiop:launch-program
   (mapcar (lambda (arg)
             (if (uiop:string-prefix-p "~/" arg)
                 (namestring (merge-pathnames (subseq arg 2) (user-homedir-pathname))) arg))
           argv)
   :input nil :output :interactive :error-output :interactive))

(defun place-app (app bundle &optional pid)
  ;; Placement failure must not break app activation or remote keyboard handoff.
  (when (and app (twigwm-apps:app-mac-region app))
    (handler-case
        (twigwm-macos-apps:place-bundle bundle (twigwm-apps:app-mac-region app) pid)
      (error (e) (format *error-output* "Window placement failed for ~a: ~a~%" bundle e)))))

(defun activate-app (app bundle)
  (twigwm-macos-apps:open-bundle bundle)
  (when (and app (twigwm-apps:app-mac-cmd app))
    (twigwm-macos-apps::output (twigwm-apps:app-mac-cmd app)))
  (place-app app bundle))

(defun service-apps ()
  (when (and *app-actions* (not (and *app-thread* (sb-thread:thread-alive-p *app-thread*))))
    (let ((action (pop *app-actions*)))
      (setf *app-thread*
            (sb-thread:make-thread
             (lambda ()
               (unwind-protect
                    (handler-case
                        (ecase (first action)
                          (:activate (apply #'activate-app (rest action)))
                          (:launch (apply #'launch-native-command (rest action)))
                          (:saved-device (apply #'twigwm-macos-apps:open-saved-device (rest action)))
                          (:previous-window (twigwm-macos-apps:previous-window))
                          (:move (apply #'twigwm-macos-apps:move-window (rest action))))
                      (error (e) (format *error-output* "App shortcut failed: ~a~%" e)))
                 (release-app-action action)))
             :name "Mac app shortcut")))))

(defstruct lease
  (lock (sb-thread:make-mutex :name "app handoff"))
  ready finished cancelled pid bundle error thread)

(defun lease-cancelled-p (lease)
  (sb-thread:with-mutex ((lease-lock lease)) (lease-cancelled lease)))

(defun lease-worker (lease)
  (unwind-protect
       (handler-case
           (unless (lease-cancelled-p lease)
             (multiple-value-bind (pid bundle)
                 (twigwm-macos-apps:open-saved-device *number-device*)
               (unless (lease-cancelled-p lease)
                 (place-app (find bundle (twigwm-apps:apps-for-mac)
                                  :key #'twigwm-apps:app-mac :test #'equal :from-end t)
                            bundle pid))
               (sb-thread:with-mutex ((lease-lock lease))
                 (setf (lease-pid lease) pid (lease-bundle lease) bundle
                       (lease-ready lease) t))))
         (error (e)
           (sb-thread:with-mutex ((lease-lock lease))
             (setf (lease-error lease) (princ-to-string e)))))
    (sb-thread:with-mutex ((lease-lock lease))
      (setf (lease-finished lease) t))))

(defun start-transfer ()
  (when (or (null *lease*) (lease-cancelled-p *lease*))
    (let ((previous *lease*) (lease (make-lease)))
      (setf *lease* lease
            (lease-thread lease)
            (sb-thread:make-thread
             (lambda ()
               ;; A cancelled focus operation must finish before the new one.
               ;; Wait on the worker, never in the keyboard callback.
               (when (and previous (lease-thread previous))
                 (sb-thread:join-thread (lease-thread previous)))
               (lease-worker lease))
             :name "desktop app handoff")))))

(defun release-events (events)
  (dolist (event events)
    (cffi:foreign-funcall "CFRelease" :pointer event :void)))

(defun cancel ()
  (setf *host-prefix-p* nil)
  (when *lease*
    (sb-thread:with-mutex ((lease-lock *lease*)) (setf (lease-cancelled *lease*) t)))
  (release-events (finish *handoff* (handoff-generation *handoff*))))

(defun event-field (event field)
  (cffi:foreign-funcall "CGEventGetIntegerValueField" :pointer event
                      :uint32 field :int64))

(defun event-flags (event)
  (cffi:foreign-funcall "CGEventGetFlags" :pointer event :uint64))

(defun event-description (event)
  (list (event-field event 9)
        (cffi:foreign-funcall "CGEventGetType" :pointer event :uint32)
        (event-flags event)))

(defun buffer-event (event &optional code type)
  (let ((copy (cffi:foreign-funcall "CGEventCreateCopy" :pointer event :pointer)))
    (when (cffi:null-pointer-p copy) (error "Cannot copy key event"))
    (handler-case
        (progn
          (when code
            (cffi:foreign-funcall "CGEventSetIntegerValueField" :pointer copy
                                :uint32 9 :int64 code :void))
          (when type
            (cffi:foreign-funcall "CGEventSetType" :pointer copy :uint32 type :void))
          (enqueue *handoff* copy))
      (error (e) (release-events (list copy)) (error e)))))

(defun command-keycodes (flags)
  ;; The number may be pressed in another Mac app. Send the held Command
  ;; modifier to Windows App before the number; its real release follows.
  (append (when (or (logtest flags #x8) (not (logtest flags #x10))) '(55))
          (when (logtest flags #x10) '(54))))

(defun start-number-transfer (event)
  (arm *handoff*)
  (dolist (code (command-keycodes (event-flags event)))
    (buffer-event event code 12))
  (buffer-event event)
  (start-transfer))

(defun dispatch-number (event)
  (if *number-device*
      (start-number-transfer event)
      (let* ((code (event-field event 9))
             (app (twigwm-apps:app-for-number (twigwm-apps:apps-for-mac) (number-key code)))
             (bundle (and app (twigwm-apps:mac-bundle app))))
        (when bundle
          (setf (gethash code *app-down*) t)
          (queue-app-action :activate app bundle)))))

(defun post-to-app (pid event)
  (cffi:foreign-funcall "CGEventPostToPid" :int pid :pointer event :void))

(defun post-event (event tag)
  ;; Re-tag the SOURCE, not just field 42: WindowServer serializes source data.
  (let ((source (cffi:foreign-funcall "CGEventSourceCreate" :int32 -1 :pointer)))
    (when (cffi:null-pointer-p source) (error "Cannot create replay source"))
    (unwind-protect
         (progn
           (cffi:foreign-funcall "CGEventSourceSetUserData" :pointer source :int64 tag :void)
           (cffi:foreign-funcall "CGEventSetSource" :pointer event :pointer source :void)
           (cffi:foreign-funcall "CGEventPost" :uint32 0 :pointer event :void))
      (release-events (list source)))))

(defun post-key-chord (pid chord)
  "Send modifier transitions to PID, or globally when PID is NIL."
  (let (events)
    (unwind-protect
         (progn
           ;; Allocate the whole chord before posting any modifier-down.
           (dolist (key chord)
             (destructuring-bind (code type flags) key
               (let ((event (cffi:foreign-funcall "CGEventCreateKeyboardEvent"
                                                 :pointer (cffi:null-pointer) :uint16 code
                                                 :unsigned-char (if (= type 10) 1 0) :pointer)))
                 (when (cffi:null-pointer-p event) (error "Cannot create chord event"))
                 (push event events)
                 (cffi:foreign-funcall "CGEventSetType" :pointer event :uint32 type :void)
                 (cffi:foreign-funcall "CGEventSetFlags" :pointer event :uint64 flags :void))))
           (dolist (event (reverse events))
             (if pid
                 (post-to-app pid event)
                 (post-event event +tab-tag+))))
      (release-events events))))

(defun dispatch-tab-switch (code type flags)
  (multiple-value-bind (pid bundle)
      (if *tab-switch* (values nil nil) (twigwm-macos-apps:frontmost))
    (multiple-value-bind (consumed target events)
        (tab-switch-step code type flags
                         (member bundle *remote-bundles* :test #'equal) pid)
      (when consumed
        (cancel)
        (post-key-chord target events)
        t))))

(defun release-tab-switch ()
  "Release a synthetic modifier if the tap exits during a switch."
  (when *tab-switch*
    (let* ((switch *tab-switch*)
           (option-p (eq (tab-switch-source switch) :option)))
      (setf *tab-switch* nil)
      (post-key-chord
       (tab-switch-pid switch)
       (append (when (tab-switch-tab-down switch) '((48 11 0)))
               (list (list (if option-p 55 58) 12 0)))))))

(defun dispatch-native-key (event pid bundle &optional local-p)
  "Consume one native action and its repeats/release, including buffered input."
  (let* ((code (event-field event 9))
         (type (cffi:foreign-funcall "CGEventGetType" :pointer event :uint32))
         (modifiers (logand (event-flags event) #x1e0000))
         (direction (movement-direction code modifiers))
         (spec (and (or bundle local-p) (native-key-spec code))))
    (when (or spec direction (gethash code *app-down*))
      (cond
        ((gethash code *app-down*)
         (when (= type 11) (remhash code *app-down*))
         t)
        ((= type 10)
         (let ((action (and spec (native-key-action spec modifiers (unless local-p bundle)))))
           (when (or (and direction (queue-window-move pid (unless local-p bundle) direction)) action)
             (setf (gethash code *app-down*) t)
             (when action
               (cond ((and (consp action) (eq (first action) :launch))
                      (apply #'queue-app-action action))
                     ((consp action) (post-key-chord pid action))
                     (t (queue-app-action :activate nil action))))
             t)))))))

(defun dispatch-host-key (event code modifiers)
  "Consume one local shortcut after Super-Escape, including unbound keys."
  (setf *host-prefix-p* nil)
  (cond
    ((number-p code modifiers) (dispatch-number event))
    (t
     (cond
       ((and (= code 53) (= modifiers *prefix-modifiers*))
        (queue-app-action :previous-window))
       ((and (= code 53) (zerop modifiers))
        ;; Like StumpWM's C-t t: send the prefix itself to the focused app.
        (post-key-chord (twigwm-macos-apps:frontmost)
                        '((55 12 #x100008) (53 10 #x100008)
                          (53 11 #x100008) (55 12 0))))
       ((and *zero-device* (= code 29) (= modifiers +command+))
        (queue-app-action :saved-device *zero-device*))
       ((or (native-key-spec code) (movement-direction code modifiers))
        (multiple-value-bind (pid bundle) (twigwm-macos-apps:frontmost)
          (dispatch-native-key event pid bundle t))))
     (setf (gethash code *app-down*) t))))

(defun service-tick ()
  (when *lease*
    (let (ready finished pid cancelled error)
      (sb-thread:with-mutex ((lease-lock *lease*))
        (setf ready (lease-ready *lease*) finished (lease-finished *lease*)
              pid (lease-pid *lease*) cancelled (lease-cancelled *lease*)
              error (lease-error *lease*)))
      (when (and ready (not cancelled) (not error) (handoff-pending *handoff*))
        (let ((events (nreverse (handoff-events *handoff*))))
          (setf (handoff-events *handoff*) nil)
          (unwind-protect
               (loop while events
                     for event = (pop events)
                     do (decf (handoff-count *handoff*))
                        (unwind-protect
                             ;; Explicit target prevents typing into a different Mac application.
                             (unless (dispatch-native-key event pid (lease-bundle *lease*))
                               (post-to-app pid event))
                          (release-events (list event))))
            (setf (handoff-events *handoff*) (nreverse events)))))
      (when finished
        (when error (format *error-output* "App handoff failed: ~a~%" error))
        (release-events (finish *handoff* (handoff-generation *handoff*)))
        (setf *lease* nil)))))

(cffi:defcallback input-event :pointer
    ((proxy :pointer) (type :uint32) (event :pointer) (context :pointer))
  (declare (ignore proxy context))
  (when (and (not (cffi:null-pointer-p event))
             (member type '(10 11 12))
             (member (event-field event 42) (list +test-tag+ +replay-tag+ +heartbeat-tag+)))
    (push (list (event-field event 42) type (event-field event 9) (event-flags event)) *probe-seen*))
  (handler-case
      (cond
        ;; Timeout/user-disable notifications are not keyboard events.
        ((member type '(#xfffffffe #xffffffff))
         (setf *callback-error* "macOS disabled the event tap")
         event)
        ((and (not (cffi:null-pointer-p event))
              (= (event-field event 42) +tab-tag+))
         event)                                    ; translated local switch, never remap again
        ((and (not (cffi:null-pointer-p event))
              (= (event-field event 42) +heartbeat-tag+))
         (setf *heartbeat* t)
         (cffi:null-pointer))
        ((and (not (cffi:null-pointer-p event))
              (= (event-field event 42) +replay-tag+))
         ;; Probe sink: observe replay at the tap, never deliver it to an app.
         (if *live* event
             (progn (push (event-description event) *replayed*) (cffi:null-pointer))))
        ((or (cffi:null-pointer-p event)
             (and (not *live*) (/= (event-field event 42) +test-tag+)))
         event)                                    ; all real input unchanged
        ((= type 22)                               ; scroll wheel, never buffered

         event)
        ((not (member type '(10 11 12)))
         (when (and *live* (member type '(1 3 25))) (cancel))
         event)                                    ; clicks cancel, never replay
        (t
         (let* ((code (event-field event 9))          ; kCGKeyboardEventKeycode
                (modifiers (logand (event-flags event) #x1e0000))
                (held (and *live* (gethash code *app-down*)))
                (native (and *live* (= type 10)
                             (or (native-key-spec code) (movement-direction code modifiers)))))
           (cond
             ((and *live* *swap-tab-modifiers*
                   (or *tab-switch* (= code 48))
                   (dispatch-tab-switch code type (event-flags event)))
              (cffi:null-pointer))
             (held
              (when (= type 11) (remhash code *app-down*))
              (cffi:null-pointer))
             ;; Even during a handoff, native Command-Tab must remain native.
             ((and (= code 48) (logtest modifiers +command+))
              (cancel)
              event)
             ((and (= code 53) *escape-down*)
              (when (= type 11) (setf *escape-down* nil))
              (cffi:null-pointer))
             ((and *live* (not *host-prefix-p*) (= type 10) (= code 53)
                   (= modifiers *prefix-modifiers*))
              (cancel)
              (setf *escape-down* t *host-prefix-p* t)
              (cffi:null-pointer))
             ((and *live* *host-prefix-p* (= type 10))
              (dispatch-host-key event code modifiers)
              (cffi:null-pointer))
             ((and (not *live*) (= type 10) (= code 53) (= modifiers *prefix-modifiers*))
              ;; The isolated event-tap probe still needs a capture trigger.
              (setf *escape-down* t)
              (arm *handoff*)
              (cffi:null-pointer))
             ((and *live* *zero-device* (= type 10) (= code 29) (= modifiers +command+))
              ;; Command-0 opens secondary directly, including from a remote session.
              (cancel)
              (setf (gethash code *app-down*) t)
              (queue-app-action :saved-device *zero-device*)
              (cffi:null-pointer))
             ((handoff-pending *handoff*)
              (cond
                ((and (= code 53) (zerop modifiers))
                 (when (= type 10) (setf *escape-down* t) (cancel)))
                (t (buffer-event event)))
              (cffi:null-pointer))
             ((and *live* (= type 10) (number-p code modifiers))
              ;; Re-selecting desktop's Mac window interrupts a nested guest's
              ;; keyboard grab. Let the focused remote session own its keys.
              (if (member (nth-value 1 (twigwm-macos-apps:frontmost))
                          *remote-bundles* :test #'equal)
                  event
                  (if (dispatch-number event) (cffi:null-pointer) event)))
             (native
              (multiple-value-bind (pid bundle) (twigwm-macos-apps:frontmost)
                (if (dispatch-native-key event pid bundle) (cffi:null-pointer) event)))
             (t event)))))
    (error (e)
      ;; Do not unwind Lisp errors through Apple's callback stack.
      (setf *callback-error* (princ-to-string e))
      (cancel)
      (ignore-errors (release-tab-switch))
      (cffi:null-pointer))))

(defun probe-event (code type flags &optional (tag +test-tag+) post)
  "Invoke the callback directly, or post tagged input to the probe's event tap."
  (let ((event (cffi:foreign-funcall "CGEventCreateKeyboardEvent"
                                  :pointer (cffi:null-pointer) :uint16 code
                                  :unsigned-char (if (= type 10) 1 0) :pointer)))
    (when (cffi:null-pointer-p event) (error "Cannot create test event"))
    (unwind-protect
         (progn
           (cffi:foreign-funcall "CGEventSetType" :pointer event :uint32 type :void)
           (cffi:foreign-funcall "CGEventSetFlags" :pointer event :uint64 flags :void)
           (cffi:foreign-funcall "CGEventSetIntegerValueField" :pointer event
                               :uint32 42 :int64 tag :void)
           (if post
               (post-event event tag)
               (values
                (cffi:null-pointer-p
                 (cffi:foreign-funcall-pointer (cffi:callback input-event) ()
                                             :pointer (cffi:null-pointer) :uint32 type
                                             :pointer event :pointer (cffi:null-pointer) :pointer))
                (event-description event))))
      (release-events (list event)))))

(defun native-buffer-check ()
  (assert (not (probe-event 18 10 0 0))) ; untagged input passes untouched
  (assert (probe-event 53 10 +command+))
  (let ((token (handoff-generation *handoff*))
        (sequence '((55 12 0) (19 10 0) (19 11 0) (18 10 0) (18 11 0))))
    (assert (probe-event 53 11 +command+))
    (dolist (event sequence) (assert (apply #'probe-event event)))
    (assert (= 5 (handoff-count *handoff*)))
    (assert (null (finish *handoff* (1- token)))) ; stale readiness cannot release
    (multiple-value-bind (events ready) (finish *handoff* token)
      (unwind-protect
           (progn
             (assert ready)
             (assert (equal sequence
                            (mapcar #'event-description events))))
        (release-events events))))
  ;; Cancelling also releases owned event copies.
  (dolist (escape '(53 48))
    (assert (probe-event 53 10 +command+))
    (assert (probe-event 53 11 +command+))
    (assert (probe-event 19 10 0))
    (assert (eq (= escape 53) (probe-event escape 10 (if (= escape 48) +command+ 0))))
    (assert (not (handoff-pending *handoff*)))
    (when (= escape 53) (assert (probe-event 53 11 0))))
  (assert (not *callback-error*)))

(defun pump-until (predicate mode)
  (loop with deadline = (+ (get-internal-real-time) (* 2 internal-time-units-per-second))
        until (funcall predicate)
        do (when *callback-error* (error "~a" *callback-error*))
           (when (> (get-internal-real-time) deadline)
             (error "Native event delivery timed out: queued ~d, replayed ~d, heartbeat ~s, tagged events ~s"
                    (handoff-count *handoff*) (length *replayed*) *heartbeat* (reverse *probe-seen*)))
           (cffi:foreign-funcall "CFRunLoopRunInMode" :pointer mode
                               :double 0.01d0 :unsigned-char 1 :int32)))

(defun native-roundtrip-check (mode)
  "Exercise real CGEventPost delivery; the tagged replay sink consumes every key."
  (let ((sequence '((55 12 0) (19 10 0) (19 11 0) (18 10 0) (18 11 0)))
        (*replayed* nil))
    ;; Prove routing with unchanged modifier flags before posting any keydowns.
    (let ((*heartbeat* nil))
      (probe-event 55 12 (cffi:foreign-funcall "CGEventSourceFlagsState" :int32 1 :uint64)
                   +heartbeat-tag+ t)
      (pump-until (lambda () *heartbeat*) mode))
    (probe-event 53 10 +command+ +test-tag+ t)
    (probe-event 53 11 +command+ +test-tag+ t)
    (dolist (event sequence) (apply #'probe-event (append event (list +test-tag+ t))))
    (pump-until (lambda () (= 5 (handoff-count *handoff*))) mode)
    (assert (not *replayed*))
    (let* ((events (finish *handoff* (handoff-generation *handoff*)))
           (expected (mapcar #'event-description events)))
      (unwind-protect
           (dolist (event events) (post-event event +replay-tag+))
        (release-events events))
      (pump-until (lambda () (= 5 (length *replayed*))) mode)
      (assert (equal expected (reverse *replayed*))))
    (format t "PASS: real CGEventPost capture and ordered replay into the test sink; no application delivery.~%")))

(defun call-with-input-tap (function)
  (unless (uiop:os-macosx-p) (error "mac-input-poc requires macOS"))
  ;; Finish CFFI registry writes before callbacks/workers resolve native symbols.
  ;; Repeated loads from the history watcher used to race with those lookups.
  (dolist (name '("ApplicationServices" "CoreFoundation" "AppKit"))
    (twigwm-macos-apps::framework name))
  (let* ((*handoff* (make-handoff))
         (*callback-error* nil)
         (*probe-seen* nil)
         (tap (cffi:foreign-funcall "CGEventTapCreate"
                                    :uint32 1 :uint32 0 :uint32 0 ; session/head/active
                                    :uint64 #x2401c0a ; keyboard, button-downs, scroll wheel
                                    :pointer (cffi:callback input-event)
                                    :pointer (cffi:null-pointer) :pointer)))
    (when (cffi:null-pointer-p tap)
      (error "Cannot create event tap; check Ciel Accessibility/Input Monitoring permissions"))
    (unwind-protect
         (let ((source (cffi:foreign-funcall "CFMachPortCreateRunLoopSource"
                                           :pointer (cffi:null-pointer) :pointer tap
                                           :long 0 :pointer))
               (run-loop (cffi:foreign-funcall "CFRunLoopGetCurrent" :pointer))
               (mode (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFRunLoopDefaultMode") :pointer)))
           (when (cffi:null-pointer-p source) (error "Cannot create event-tap run-loop source"))
           (unwind-protect
                (progn
                  (cffi:foreign-funcall "CFRunLoopAddSource" :pointer run-loop
                                      :pointer source :pointer mode :void)
                  (cffi:foreign-funcall "CGEventTapEnable" :pointer tap :unsigned-char 1 :void)
                  (assert (plusp (cffi:foreign-funcall "CGEventTapIsEnabled"
                                                     :pointer tap :unsigned-char)))
                  (funcall function mode))
             (cffi:foreign-funcall "CFRunLoopRemoveSource" :pointer run-loop
                                 :pointer source :pointer mode :void)
             (release-events (list source))))
      (cancel)
      (cffi:foreign-funcall "CFMachPortInvalidate" :pointer tap :void)
      (release-tab-switch)
      (release-events (list tap)))))

(defun probe ()
  "Bounded native test; only tagged input is captured, replay goes to a test sink."
  (call-with-input-tap
   (lambda (mode)
     (native-buffer-check)
     (setf *probe-seen* nil)
     (native-roundtrip-check mode)
     (assert (not *callback-error*))
     (format t "PASS: native event tap and C callback; explicit readiness and cancellation.~%"))))

(defun run (&key seconds (modifiers +command+))
  "Resident app shortcuts; SECONDS bounds a trial, NIL runs until stopped."
  (let* ((*live* t) (*lease* nil) (*prefix-modifiers* modifiers) (*escape-down* nil)
         (*tab-switch* nil)
         (*host-prefix-p* nil)
         (apps (twigwm-apps:apps-for-mac))
         (*remote-bundles* (twigwm-apps:mac-passthrough-bundles apps))
         (history-stop (sb-thread:make-semaphore)) (history-thread nil)
         (*app-down* (make-hash-table)) (*app-actions* nil) (*app-thread* nil))
    (twigwm-macos-apps:reset-placements)
    (twigwm-macos-apps:reset-window-history)
    (unwind-protect
     (call-with-input-tap
     (lambda (mode)
       (setf history-thread
             (sb-thread:make-thread
              (lambda ()
                (loop do (ignore-errors (twigwm-macos-apps:record-front-window))
                      until (sb-thread:wait-on-semaphore history-stop :timeout 0.1)))
              :name "Mac focused window history"))
       (format t "READY: native app shortcuts (~x); ~a.~%" modifiers
               (if seconds "bounded trial" "resident"))
       (finish-output)
       (loop with end = (and seconds (+ (get-internal-real-time)
                                       (* seconds internal-time-units-per-second)))
             while (or (not end) (< (get-internal-real-time) end))
             do (when *callback-error* (error "Event tap failed: ~a" *callback-error*))
                (service-tick)
                (service-apps)
                (cffi:foreign-funcall "CFRunLoopRunInMode" :pointer mode
                                    :double 0.01d0 :unsigned-char 1 :int32))))
      (when *app-thread* (sb-thread:join-thread *app-thread*))
      (sb-thread:signal-semaphore history-stop)
      (when history-thread (sb-thread:join-thread history-thread))
      (dolist (action *app-actions*) (release-app-action action))
      (twigwm-macos-apps:reset-window-history)
      (twigwm-macos-apps:reset-placements))))
