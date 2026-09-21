;;;; Native app focus/placement and desktop guest submission for the input service.
(defpackage :twigwm-macos-apps
  (:use :cl)
  (:shadow #:class)
  (:export #:focus #:rdp-pid #:submit #:nonce #:await
           #:frontmost #:open-bundle #:focus-windows #:open-saved-device #:place-bundle
           #:movable-window #:move-window #:reset-placements
           #:record-front-window #:previous-window #:reset-window-history
           #:screens #:*displays* #:*regions* #:*guest-command*))
(in-package :twigwm-macos-apps)

(defvar *guest-command* nil)
(defvar *window-title* "desktop")
(defparameter *command-timeout* 15)

(defun await (ready seconds context)
  "Check immediately; wait only while pending, with a bounded deadline."
  (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        until (funcall ready)
        do (when (>= (get-internal-real-time) deadline)
             (error "~a timed out; not retried" context))
           (sleep 0.01)))

(defun call (argv)
  "Run argv once, supervising the child in Lisp; return stdout/stderr/status."
  ;; Files avoid pipe deadlocks while waiting, and are removed on every exit.
  (uiop:with-temporary-file (:stream out :direction :io)
    (uiop:with-temporary-file (:stream err :direction :io)
      (let ((process (uiop:launch-program argv :input nil :output out :error-output err))
            (status nil))
        (unwind-protect
             (await (lambda () (not (uiop:process-alive-p process)))
                       *command-timeout* (first argv))
          (when (uiop:process-alive-p process)
            (uiop:terminate-process process :urgent t))
          (setf status (uiop:wait-process process)))
        (file-position out 0)
        (file-position err 0)
        (values (uiop:slurp-stream-string out) (uiop:slurp-stream-string err) status)))))

(defun output (argv)
  (multiple-value-bind (out err status) (call argv)
    (unless (and (eql status 0) (string= err ""))
      (error "~a failed (~a): ~a~a" (first argv) status err out))
    out))

(defun submit (form ack)
  ;; SSH waits for the guest helper's acknowledgement. Never replay a command
  ;; after a transport failure: it may already have taken effect in StumpWM.
  (let ((reply
          (output
           (append (or *guest-command* (error "No guest command configured")) (list form ack)))))
    (unless (string= (string-right-trim '(#\Newline #\Return) reply) ack)
      (error "Unexpected guest reply; not retried: ~a" reply))))

;; Direct Accessibility avoids System Events' menu-click AppleEvent timeout.
;; Enable /opt/homebrew/bin/ciel in macOS Privacy & Security > Accessibility.
;; Load native frameworks on demand, so the helper can be tested on Linux.
(defvar *frameworks* (make-hash-table :test #'equal))
(defvar *framework-lock* (sb-thread:make-mutex :name "Mac frameworks"))

(defun framework (name)
  (sb-thread:with-mutex (*framework-lock*)
    (or (gethash name *frameworks*)
        (setf (gethash name *frameworks*)
              (cffi:load-foreign-library
               (format nil "/System/Library/Frameworks/~a.framework/~a" name name))))))

(defmacro with-cf ((name value) &body body)
  `(let ((,name ,value))
     (when (cffi:null-pointer-p ,name)
       (error "Native operation returned no ~a" ',name))
     (unwind-protect (progn ,@body)
       (cffi:foreign-funcall "CFRelease" :pointer ,name :void))))

(defun cf-string (text)
  (cffi:foreign-funcall "CFStringCreateWithCString" :pointer (cffi:null-pointer)
                      :string text :uint32 #x08000100 :pointer))

(defun cf-equal (a b)
  (plusp (cffi:foreign-funcall "CFEqual" :pointer a :pointer b :unsigned-char)))

(defun cf-items (array)
  "Borrow array elements; the caller must keep ARRAY alive while using them."
  (loop for i below (cffi:foreign-funcall "CFArrayGetCount" :pointer array :long)
        collect (cffi:foreign-funcall "CFArrayGetValueAtIndex" :pointer array :long i :pointer)))

(defun only (items context)
  (unless (= (length items) 1)
    (error "Expected one ~a, found ~d" context (length items)))
  (first items))

(defun cf-text (string)
  (let ((size (1+ (* 4 (cffi:foreign-funcall "CFStringGetLength" :pointer string :long)))))
    (cffi:with-foreign-object (buffer :char size)
      (unless (plusp (cffi:foreign-funcall "CFStringGetCString" :pointer string
                                         :pointer buffer :long size :uint32 #x08000100 :unsigned-char))
        (error "Cannot read Accessibility window title"))
      (cffi:foreign-string-to-lisp buffer :encoding :utf-8))))

(defun ax-check (code operation)
  (unless (zerop code)
    (error "Accessibility ~a failed (~d)" operation code)))

(defun ax-timeout (element seconds)
  (ax-check (cffi:foreign-funcall "AXUIElementSetMessagingTimeout"
                                   :pointer element :float seconds :int) "timeout"))

(defun ax-get (element name)
  (with-cf (key (cf-string name))
    (cffi:with-foreign-object (value :pointer)
      (ax-check
       (cffi:foreign-funcall "AXUIElementCopyAttributeValue"
                            :pointer element :pointer key :pointer value :int)
       name)
      (cffi:mem-ref value :pointer))))

(defun ax-set (element name value)
  (with-cf (key (cf-string name))
    (ax-check
     (cffi:foreign-funcall "AXUIElementSetAttributeValue"
                          :pointer element :pointer key :pointer value :int)
     name)))

(defun ax-text (element name)
  (with-cf (text (ax-get element name)) (cf-text text)))

(defun ax-title (element) (ax-text element "AXTitle"))

(defun ax-child (parent name)
  "Return an owned reference to the uniquely named Accessibility child."
  (with-cf (children (ax-get parent "AXChildren"))
    (let* ((entries (mapcar (lambda (child) (cons (ax-title child) child))
                            (cf-items children)))
           (match (only (remove name entries :key #'car :test-not #'string=)
                           (format nil "Accessibility item ~s in ~s" name (mapcar #'car entries)))))
      (cffi:foreign-funcall "CFRetain" :pointer (cdr match) :pointer))))

(defun selector (name)
  (cffi:foreign-funcall "sel_registerName" :string name :pointer))

(defun class (name)
  (cffi:foreign-funcall "objc_getClass" :string name :pointer))

(defun send (object selector &optional (argument nil argument-p))
  (if argument-p
      (cffi:foreign-funcall "objc_msgSend" :pointer object
                          :pointer (selector selector) :pointer argument :pointer)
      (cffi:foreign-funcall "objc_msgSend" :pointer object
                          :pointer (selector selector) :pointer)))

(defmacro with-pool (&body body)
  `(progn
     (framework "AppKit")
     (let ((pool (send (class "NSAutoreleasePool") "new")))
       (unwind-protect (progn ,@body)
         (cffi:foreign-funcall "objc_msgSend" :pointer pool
                             :pointer (selector "drain") :void)))))

(defun app-pid (bundle-name)
  ;; Resolve the running application by bundle identity, entirely in-process.
  (with-pool
           (with-cf (bundle (cf-string bundle-name))
             ;; NSArray/NSString are toll-free bridged to CFArray/CFString.
             (let ((apps (cffi:foreign-funcall "objc_msgSend"
                                              :pointer (class "NSRunningApplication")
                                              :pointer (selector "runningApplicationsWithBundleIdentifier:")
                                              :pointer bundle :pointer)))
               (cffi:foreign-funcall "objc_msgSend"
                                    :pointer (only (cf-items apps) bundle-name)
                                    :pointer (selector "processIdentifier") :int)))))

(defun rdp-pid () (app-pid "com.microsoft.rdc.macos"))

(defun frontmost ()
  "Return the frontmost application's PID and bundle ID without a subprocess."
  (with-pool
    (let* ((workspace (send (class "NSWorkspace") "sharedWorkspace"))
           (app (send workspace "frontmostApplication"))
           (bundle (send app "bundleIdentifier")))
      (values (cffi:foreign-funcall "objc_msgSend" :pointer app
                                   :pointer (selector "processIdentifier") :int)
              (unless (cffi:null-pointer-p bundle) (cf-text bundle))))))

(defun open-bundle (bundle-name)
  "Ask LaunchServices to open/raise an installed native app or Chrome PWA."
  (with-pool
    (let ((workspace (send (class "NSWorkspace") "sharedWorkspace")))
      (with-cf (bundle (cf-string bundle-name))
        (let ((url (cffi:foreign-funcall "objc_msgSend" :pointer workspace
                                       :pointer (selector "URLForApplicationWithBundleIdentifier:")
                                       :pointer bundle :pointer)))
          (when (cffi:null-pointer-p url) (error "Application not installed: ~a" bundle-name))
          (unless (plusp (cffi:foreign-funcall "objc_msgSend" :pointer workspace
                                             :pointer (selector "openURL:")
                                             :pointer url :unsigned-char))
            (error "Cannot open application: ~a" bundle-name)))))))

(defun session-title (menu)
  "First existing session after Connection Center in Windows App's Window menu."
  ;; AXWindows can omit an existing remote console. The Window menu remains
  ;; authoritative for existing remote sessions.
  (with-cf (items (ax-get menu "AXChildren"))
    (loop with sessions = nil
          for item in (cf-items items)
          for title = (ax-title item)
          when (and sessions (plusp (length title)))
            do (with-cf (enabled (ax-get item "AXEnabled"))
                 (when (cf-equal enabled (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFBooleanTrue") :pointer))
                   (return title)))
          when (equal title "Connection Center") do (setf sessions t)
          finally (error "Windows App has no active session in its Window menu"))))

(defun focus (&optional (bundle "com.microsoft.rdc.macos") (title *window-title*) window-title)
  (framework "ApplicationServices")
  (framework "CoreFoundation")
  (unless (plusp (cffi:foreign-funcall "AXIsProcessTrusted" :unsigned-char))
    (error "Enable /opt/homebrew/bin/ciel in macOS Privacy & Security > Accessibility"))
  ;; Process-local default covers every menu/window object returned by AX.
  (with-cf (system (cffi:foreign-funcall "AXUIElementCreateSystemWide" :pointer))
    (ax-check (cffi:foreign-funcall "AXUIElementSetMessagingTimeout"
                                     :pointer system :float 1.0 :int) "timeout"))
  (let ((pid (app-pid bundle))
        (true (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFBooleanTrue") :pointer)))
    (unless (plusp pid) (error "Invalid application process ID"))
    (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication" :int pid :pointer))
      ;; The Window menu identifies the exact remote session and can raise it.
      (with-cf (menubar (ax-get app "AXMenuBar"))
        (with-cf (window-menu (ax-child menubar "Window"))
          (with-cf (menus (ax-get window-menu "AXChildren"))
            (let ((menu (only (cf-items menus) "Window menu")))
              (unless title (setf title (session-title menu)))
            (with-cf (item (ax-child menu title))
              (ax-set app "AXFrontmost" true)
              (with-cf (action (cf-string "AXPress"))
                (ax-check (cffi:foreign-funcall "AXUIElementPerformAction"
                                                 :pointer item :pointer action :int) "menu press")))))))
      ;; Window creation/activation is asynchronous. Check immediately; wait
      ;; only while pending, never repeat the menu action or guest dispatch.
      (await (lambda ()
                  (with-cf (frontmost (ax-get app "AXFrontmost"))
                    (and (cf-equal frontmost true)
                         (with-cf (focused (ax-get app "AXFocusedWindow"))
                           (string= (ax-title focused) (or window-title title))))))
                1 "session window focus"))
    (values pid bundle)))

(defun focus-windows (&optional title)
  (open-bundle "com.microsoft.rdc.macos")
  (focus "com.microsoft.rdc.macos" title))

(defun open-saved-device (name)
  "Open the saved device, wait for its window, and return its PID and bundle."
  (open-bundle "com.microsoft.rdc.macos")
  (framework "ApplicationServices")
  ;; An open connection belongs to the Window menu. Pressing its saved-device
  ;; card needlessly raises Connection Center and may leave that window visible.
  (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication"
                                       :int (app-pid "com.microsoft.rdc.macos") :pointer))
    (with-cf (menubar (ax-get app "AXMenuBar"))
      (with-cf (window-menu (ax-child menubar "Window"))
        (with-cf (menus (ax-get window-menu "AXChildren"))
          (with-cf (items (ax-get (only (cf-items menus) "Window menu") "AXChildren"))
            (when (member name (cf-items items) :key #'ax-title :test #'equal)
              (return-from open-saved-device
                (focus "com.microsoft.rdc.macos" name))))))))
  ;; The menu calls this Connection Center; its window is titled Windows App.
  (let ((pid (focus "com.microsoft.rdc.macos" "Connection Center" "Windows App")))
    (labels ((find-description (node description)
               (if (equal (ignore-errors (ax-text node "AXDescription")) description)
                   (cffi:foreign-funcall "CFRetain" :pointer node :pointer)
                   (ignore-errors
                     (with-cf (children (ax-get node "AXChildren"))
                       (some (lambda (child) (find-description child description))
                             (cf-items children))))))
             (require-description (node description)
               (or (find-description node description)
                   (error "Windows App saved device control not found: ~a" description))))
      (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication" :int pid :pointer))
        (with-cf (window (ax-get app "AXFocusedWindow"))
          (with-cf (devices (require-description window "Saved Devices"))
            (with-cf (device (require-description devices name))
              (with-cf (action (cf-string "AXPress"))
                (ax-check (cffi:foreign-funcall "AXUIElementPerformAction"
                                                 :pointer device :pointer action :int)
                             "open saved device"))))))
      (await
       (lambda ()
         (ignore-errors
           (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication" :int pid :pointer))
             (with-cf (window (ax-get app "AXFocusedWindow"))
               (string= (ax-title window) name)))))
       4 "saved device window focus")
      ;; Select the connection explicitly after activation, too: a remembered
      ;; AXFocusedWindow alone does not establish which window is in front.
      (focus "com.microsoft.rdc.macos" name))))

;;; Physical displays are frame groups; placement runs only on a shortcut.
;;; UUID, minimum top clearance (logical points). Keep the manually chosen
;;; external menu-bar clearance even when visibleFrame reports the whole screen.
(defvar *displays* '((:main :primary 0)))
(defvar *regions* '((:main :main 0 0 1 1)))

;; Retained AX identities distinguish windows even when their titles change.
;; No disk state: releasing these references on service restart restores defaults.
(defvar *placements* nil)
(defvar *placement-lock* (sb-thread:make-mutex :name "Mac placements"))

(defun reset-placements ()
  (sb-thread:with-mutex (*placement-lock*)
    (dolist (entry *placements*)
      (cffi:foreign-funcall "CFRelease" :pointer (car entry) :void))
    (setf *placements* nil)))

(defun window-region (window default)
  (sb-thread:with-mutex (*placement-lock*)
    (or (cdr (assoc window *placements* :test #'cf-equal)) default)))

(defun remember-region (window region)
  (sb-thread:with-mutex (*placement-lock*)
    (let ((entry (assoc window *placements* :test #'cf-equal)))
      (if entry (setf (cdr entry) region)
          (push (cons (cffi:foreign-funcall "CFRetain" :pointer window :pointer) region)
                *placements*)))))

(defun region-rect (region screens)
  "AX (x y width height), or NIL if the assigned display is disconnected.
SCREENS contains (UUID bounds visible-bounds), all in logical AX coordinates."
  (when region
    (destructuring-bind (role left top right bottom)
        (or (cdr (assoc region *regions*)) (error "Unknown Mac region: ~s" region))
      (destructuring-bind (uuid clearance) (cdr (assoc role *displays*))
        (let ((screen (if (eq uuid :primary) (first screens) (assoc uuid screens :test #'string-equal))))
          (when screen
            (destructuring-bind (x y width height) (third screen)
              (let* ((inset (max 0 (- (+ (second (second screen)) clearance) y)))
                     (usable-height (- height inset))
                     (x0 (round (* width left)))
                     (y0 (round (* usable-height top))))
                (when (plusp usable-height)
                  (list (+ x x0) (+ y inset y0)
                        (- (round (* width right)) x0)
                        (- (round (* usable-height bottom)) y0)))))))))))

(defun screen-rect (screen selector primary-height)
  ;; NSInvocation handles CGRect's struct-return ABI on both ARM and Intel.
  (let* ((sel (selector selector))
         (signature (send screen "methodSignatureForSelector:" sel))
         (call (send (class "NSInvocation") "invocationWithMethodSignature:" signature)))
    (send call "setTarget:" screen)
    (send call "setSelector:" sel)
    (send call "invoke")
    (cffi:with-foreign-object (rect :double 4)
      (send call "getReturnValue:" rect)
      (destructuring-bind (x y width height)
          (loop for i below 4 collect (round (cffi:mem-aref rect :double i)))
        ;; Cocoa has a bottom-left origin; AX has a top-left origin.
        (list x (- primary-height y height) width height)))))

(defun neighbor-region (rect direction screens)
  "Find the current region by overlap, then its nearest aligned neighbor.
No wraparound or diagonal jumps; monitor gaps and negative origins are allowed."
  (labels ((overlap (a b axis)
             (max 0 (- (min (+ (nth axis a) (nth (+ axis 2) a))
                            (+ (nth axis b) (nth (+ axis 2) b)))
                       (max (nth axis a) (nth axis b)))))
           (center (r axis) (+ (nth axis r) (/ (nth (+ axis 2) r) 2)))
           (distance (a b)
             (+ (expt (- (center a 0) (center b 0)) 2)
                (expt (- (center a 1) (center b 1)) 2))))
    (let* ((regions (loop for (name) in *regions*
                          for bounds = (region-rect name screens)
                          when bounds collect (cons name bounds)))
           (current (first (setf regions (stable-sort regions
                            (lambda (a b)
                              (let ((oa (* (overlap rect (cdr a) 0) (overlap rect (cdr a) 1)))
                                    (ob (* (overlap rect (cdr b) 0) (overlap rect (cdr b) 1))))
                                (if (= oa ob) (< (distance rect (cdr a)) (distance rect (cdr b)))
                                    (> oa ob))))))))
           (axis (ecase direction ((:left :right) 0) ((:up :down) 1)))
           (cross (- 1 axis))
           (positive (member direction '(:right :down)))
           (best nil) (best-gap nil) (best-overlap nil) (best-offset nil))
      (when current
        (dolist (candidate regions)
          (let* ((a (cdr current)) (b (cdr candidate))
                 (gap (if positive (- (nth axis b) (+ (nth axis a) (nth (+ axis 2) a)))
                          (- (nth axis a) (+ (nth axis b) (nth (+ axis 2) b)))))
                 (shared (overlap a b cross))
                 (offset (abs (- (center a cross) (center b cross)))))
            (when (and (not (eq candidate current)) (>= gap 0) (plusp shared)
                       (or (null best-gap) (< gap best-gap)
                           (and (= gap best-gap)
                                (or (> shared best-overlap)
                                    (and (= shared best-overlap) (< offset best-offset))))))
              (setf best (car candidate) best-gap gap best-overlap shared best-offset offset)))))
      (values best (car current)))))

(defun screens ()
  (with-pool
    (let* ((screens (cf-items (send (class "NSScreen") "screens")))
           (height (fourth (screen-rect (first screens) "frame" 0))))
      (loop for screen in screens collect
        (with-cf (key (cf-string "NSScreenNumber"))
          (let* ((number (cffi:foreign-funcall "CFDictionaryGetValue"
                                             :pointer (send screen "deviceDescription")
                                             :pointer key :pointer))
                 (id (cffi:foreign-funcall "objc_msgSend" :pointer number
                                         :pointer (selector "unsignedIntValue") :uint32)))
            (with-cf (uuid (cffi:foreign-funcall "CGDisplayCreateUUIDFromDisplayID"
                                                 :uint32 id :pointer))
              (with-cf (name (cffi:foreign-funcall "CFUUIDCreateString"
                                                   :pointer (cffi:null-pointer) :pointer uuid :pointer))
                (list (cf-text name)
                      (screen-rect screen "frame" height)
                      (screen-rect screen "visibleFrame" height))))))))))

(defun ax-flag-p (window name)
  (with-cf (value (ax-get window name))
    (plusp (cffi:foreign-funcall "CFBooleanGetValue" :pointer value :unsigned-char))))

(defun ax-set-pair (window name type values)
  (cffi:with-foreign-object (pair :double 2)
    (loop for value in values for i from 0
          do (setf (cffi:mem-aref pair :double i) (coerce value 'double-float)))
    (with-cf (value (cffi:foreign-funcall "AXValueCreate" :int type :pointer pair :pointer))
      (ax-set window name value))))

(defun ax-pair (window name type)
  (with-cf (value (ax-get window name))
    (cffi:with-foreign-object (pair :double 2)
      (unless (plusp (cffi:foreign-funcall "AXValueGetValue" :pointer value :int type
                                         :pointer pair :unsigned-char))
        (error "Cannot read ~a" name))
      (loop for i below 2 collect (cffi:mem-aref pair :double i)))))

(defun movable-window (pid remote-p)
  "Return an owned focused window, or NIL to pass input through.
The tap never polls AX: a short IPC timeout fails open on unresponsive apps."
  (handler-case
      (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication" :int pid :pointer))
        (ax-timeout app 0.02)
        (with-cf (window (ax-get app "AXFocusedWindow"))
          (ax-timeout window 0.02)
          (unless (and remote-p (ax-flag-p window "AXFullScreen"))
            (cffi:foreign-funcall "CFRetain" :pointer window :pointer))))
    (error () nil)))

(defun place-window (window rect)
  ;; Never exit native fullscreen/Spaces, resize a sheet, or unminimize a window.
  (cond ((ax-flag-p window "AXFullScreen") :fullscreen)
        ((ax-flag-p window "AXMinimized") :minimized)
        ((not (string= (ax-text window "AXSubrole") "AXStandardWindow")) :nonstandard)
        (t
         (let ((position (subseq rect 0 2)) (size (subseq rect 2))
               (actual nil) (failure nil))
           ;; Resizing can change the position, and crossing displays can clamp
           ;; the size. Reapply both, then check what the application accepted.
           ;; Smaller cell-sized windows are fine; spilling outside is not.
           (loop for attempt below 3 do
             (handler-case
                 (progn
                   (ax-set-pair window "AXSize" 2 size)
                   (ax-set-pair window "AXPosition" 1 position)
                   (ax-set-pair window "AXSize" 2 size)
                   (ax-set-pair window "AXPosition" 1 position)
                   (setf actual (append (ax-pair window "AXPosition" 1)
                                        (ax-pair window "AXSize" 2))
                         failure nil)
                   (when (and (every #'= position (subseq actual 0 2))
                              (every #'plusp (subseq actual 2))
                              (every #'<= (subseq actual 2) size))
                     (return-from place-window :placed)))
               (error (e) (setf failure e)))
             (when (< attempt 2) (sleep 0.05)))
           (error "Window did not fit ~S; actual bounds ~S~@[; ~A~]"
                  rect actual failure)))))

;;; MRU window identities, including separate windows owned by the same app.
;;; Only the background focus watcher and app-action worker use this lock.
(defvar *window-history* nil)
(defvar *window-history-lock* (sb-thread:make-mutex :name "Mac window history"))

(defun reset-window-history ()
  (sb-thread:with-mutex (*window-history-lock*)
    (dolist (entry *window-history*)
      (cffi:foreign-funcall "CFRelease" :pointer (car entry) :void))
    (setf *window-history* nil)))

(defun note-window (window pid)
  "Caller holds the history lock; retain each window identity only once."
  (let ((entry (assoc window *window-history* :test #'cf-equal)))
    (unless entry
      (setf entry (cons (cffi:foreign-funcall "CFRetain" :pointer window :pointer) pid)))
    (setf *window-history* (cons entry (remove entry *window-history* :test #'eq)))))

(defun record-front-window ()
  "Record only a responsive, focused standard window. Never move it."
  (sb-thread:with-mutex (*window-history-lock*)
    (let* ((pid (frontmost)) (window (movable-window pid nil)))
      (when window
        (with-cf (selected window)
          (when (equal (ax-text selected "AXSubrole") "AXStandardWindow")
            (note-window selected pid)
            t))))))

(defun raise-window (window pid)
  "Raise the exact existing window without resizing or launching an application."
  (ax-timeout window 0.2)
  (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication" :int pid :pointer))
    (ax-timeout app 0.2)
    (let ((true (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFBooleanTrue") :pointer))
          (false (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFBooleanFalse") :pointer)))
      (when (ax-flag-p window "AXMinimized")
        (ax-set window "AXMinimized" false))
      (ax-set app "AXFrontmost" true)
      (with-cf (action (cf-string "AXRaise"))
        (ax-check (cffi:foreign-funcall "AXUIElementPerformAction"
                                         :pointer window :pointer action :int) "raise window"))
      (await (lambda ()
                  (and (ax-flag-p app "AXFrontmost")
                       (with-cf (focused (ax-get app "AXFocusedWindow"))
                         (cf-equal focused window))))
                1 "previous window focus"))))

(defun previous-window ()
  ;; Refresh now so a rapid switch followed by the prefix uses the current window.
  (when (record-front-window)
    (sb-thread:with-mutex (*window-history-lock*)
      (loop for entry = (second *window-history*)
            while entry
            do (handler-case
                   (progn
                     (raise-window (car entry) (cdr entry))
                     (note-window (car entry) (cdr entry))
                     (return t))
                 (error ()
                   ;; Closed windows and exited apps cannot be selected again.
                   (setf *window-history* (remove entry *window-history* :test #'eq))
                   (cffi:foreign-funcall "CFRelease" :pointer (car entry) :void)))))))

(defun move-window (window direction)
  (ax-timeout window 0.2)
  (let* ((screens (screens))
         (rect (append (ax-pair window "AXPosition" 1) (ax-pair window "AXSize" 2))))
    (multiple-value-bind (neighbor current) (neighbor-region rect direction screens)
      ;; Even at an outer edge, restore a manually moved or oversized window to
      ;; its tile instead of leaving it between regions or outside the screen.
      (let ((region (or neighbor current)))
        (when (and region (eq :placed (place-window window (region-rect region screens))))
          (remember-region window region))))))

(defun place-bundle (bundle region &optional pid)
  "Place only the selected window after launch/frame selection, without refocusing."
  (framework "ApplicationServices")
  (let ((window nil))
    (unwind-protect
         (progn
           ;; App launches are asynchronous. Poll readiness, never repeat a move.
           (await
            (lambda ()
              (ignore-errors
                (with-cf (app (cffi:foreign-funcall "AXUIElementCreateApplication"
                                                    :int (or pid (app-pid bundle)) :pointer))
                  (ax-timeout app 0.2)
                  (when (ax-flag-p app "AXFrontmost")
                    (setf window (ax-get app "AXFocusedWindow"))))))
            2 "selected app window")
           (let ((rect (region-rect (window-region window region) (screens))))
             (if rect (place-window window rect) :no-display)))
      (when window (cffi:foreign-funcall "CFRelease" :pointer window :void)))))

(defun nonce ()
  (framework "CoreFoundation")
  (with-cf (uuid (cffi:foreign-funcall "CFUUIDCreate" :pointer (cffi:null-pointer) :pointer))
    (with-cf (text (cffi:foreign-funcall "CFUUIDCreateString"
                                         :pointer (cffi:null-pointer) :pointer uuid :pointer))
      (cf-text text))))
