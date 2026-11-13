;;;; Menu-bar status item: a clock title and a menu of actions with their shortcuts.
;;;; Copyright (c) 2026 TwigWM contributors. MIT License.
(defpackage :twigwm-macos-menubar
  (:use :cl)
  (:export #:show #:hide #:pump #:clock #:clock-runs #:key-equivalent #:*clock-format*
           #:*clock-colors* #:*clock-font*))
(in-package :twigwm-macos-menubar)

(defvar *clock-format* "%a %H:%M:%S"
  "strftime format of the menu-bar clock, or a list of formats whose runs
*CLOCK-COLORS* colours in turn.")

(defvar *clock-colors* nil
  "List of (RED GREEN BLUE), each 0 to 255, that colour the runs of the clock
in turn; NIL leaves it the menu bar's colour.")

(defvar *clock-font* nil
  "(NAME SIZE) of the menu-bar clock's font, such as (\"TerminusTTF\" 14); a NIL
SIZE means the menu bar's, and NIL the monospaced system font.")

(defvar *item* nil "The NSStatusItem SHOW put in the menu bar, or NIL.")
(defvar *timer* nil "The CFRunLoopTimer that redraws the clock every second.")
(defvar *actions* #() "Functions of the shown menu's items, indexed by item tag.")
(defvar *chosen* nil "Tags of items chosen since PUMP last ran their actions.")

(defparameter *function-keys*
  '((123 . #xf702) (124 . #xf703) (125 . #xf701) (126 . #xf700) (53 . #x1b)
    (122 . #xf704) (120 . #xf705) (99 . #xf706) (118 . #xf707) (96 . #xf708)
    (97 . #xf709) (98 . #xf70a) (100 . #xf70b) (101 . #xf70c) (109 . #xf70d)
    (103 . #xf70e) (111 . #xf70f) (105 . #xf710) (107 . #xf711) (113 . #xf712)
    (106 . #xf713) (64 . #xf714) (79 . #xf715) (80 . #xf716) (90 . #xf717))
  "AppKit key-equivalent characters for the virtual keycodes a keyboard layout
does not name: the arrows, Escape and F1 to F20.")

(defmacro msg (object name &rest arguments)
  "Send message NAME to OBJECT. ARGUMENTS are CFFI type and value pairs followed
by the return type, as for cffi:foreign-funcall."
  `(cffi:foreign-funcall "objc_msgSend" :pointer ,object
                         :pointer (twigwm-macos-apps::selector ,name) ,@arguments))

(defmacro with-appkit (&body body)
  "Run BODY with SBCL's floating-point traps masked: AppKit raises invalid and
inexact operations internally, which SBCL would otherwise signal as errors."
  `(sb-int:with-float-traps-masked (:invalid :inexact :overflow :underflow :divide-by-zero)
     ,@body))

(defun objc-class (name)
  "The Objective-C class NAME, or a null pointer."
  (twigwm-macos-apps::class name))

(defun clock (format)
  "The local time as strftime FORMAT, in the C locale this process keeps."
  (cffi:with-foreign-objects ((now :long) (tm :char 64) (text :char 64))
    (setf (cffi:mem-ref now :long)
          (cffi:foreign-funcall "time" :pointer (cffi:null-pointer) :long))
    (cffi:foreign-funcall "localtime_r" :pointer now :pointer tm :pointer)
    (cffi:foreign-funcall "strftime" :pointer text :size 64 :string format :pointer tm :size)
    (cffi:foreign-string-to-lisp text)))

(defun clock-runs ()
  "The clock as a list of (TEXT . COLOR), one per *CLOCK-FORMAT* run: CLOCK of the
run and its colour from *CLOCK-COLORS* in turn, or NIL."
  (loop for format in (if (listp *clock-format*) *clock-format* (list *clock-format*))
        for i from 0
        collect (cons (clock format)
                      (and *clock-colors* (nth (mod i (length *clock-colors*)) *clock-colors*)))))

(defun key-equivalent (code)
  "The NSMenuItem key equivalent for virtual keycode CODE: an AppKit function-key
character, else what the current keyboard layout types for CODE."
  (let ((special (cdr (assoc code *function-keys*))))
    (if special
        (string (code-char special))
        (let ((event (cffi:foreign-funcall "CGEventCreateKeyboardEvent" :pointer (cffi:null-pointer)
                                           :uint16 code :unsigned-char 1 :pointer)))
          (unwind-protect
               (cffi:with-foreign-objects ((length :unsigned-long) (text :uint16 4))
                 (cffi:foreign-funcall "CGEventKeyboardGetUnicodeString" :pointer event
                                       :unsigned-long 4 :pointer length :pointer text :void)
                 (if (plusp (cffi:mem-ref length :unsigned-long))
                     (string (code-char (cffi:mem-aref text :uint16 0)))
                     ""))
            (cffi:foreign-funcall "CFRelease" :pointer event :void))))))

(defun clock-font ()
  "The NSFont *CLOCK-FONT* names, else, also when it is not installed, the
monospaced system font, at its size or the menu bar's."
  (let ((size (float (or (second *clock-font*)
                         (msg (msg (objc-class "NSFont") "menuBarFontOfSize:" :double 0d0 :pointer)
                              "pointSize" :double))
                     1d0)))
    (or (and *clock-font*
             (twigwm-macos-apps::with-cf (name (twigwm-macos-apps::cf-string (first *clock-font*)))
               (let ((font (msg (objc-class "NSFont") "fontWithName:size:"
                                :pointer name :double size :pointer)))
                 (unless (cffi:null-pointer-p font) font))))
        (msg (objc-class "NSFont") "monospacedSystemFontOfSize:weight:"
             :double size :double 0d0 :pointer))))

(defun appkit-constant (name)
  "The AppKit global NAME, such as the string constant NSFontAttributeName."
  (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer))

(defun clock-title ()
  "An autoreleased NSAttributedString of CLOCK-RUNS in CLOCK-FONT, each run in its
sRGB colour."
  (let ((title (msg (msg (msg (objc-class "NSMutableAttributedString") "alloc" :pointer)
                         "init" :pointer)
                    "autorelease" :pointer))
        (font (clock-font)))
    (loop for (text red green blue) in (clock-runs)
          do (cffi:with-foreign-objects ((keys :pointer 2) (objects :pointer 2))
               (setf (cffi:mem-aref keys :pointer 0) (appkit-constant "NSFontAttributeName")
                     (cffi:mem-aref keys :pointer 1) (appkit-constant "NSForegroundColorAttributeName")
                     (cffi:mem-aref objects :pointer 0) font
                     (cffi:mem-aref objects :pointer 1)
                     (msg (objc-class "NSColor") "colorWithSRGBRed:green:blue:alpha:"
                          :double (/ red 255d0) :double (/ green 255d0) :double (/ blue 255d0)
                          :double 1d0 :pointer))
               (twigwm-macos-apps::with-cf (string (twigwm-macos-apps::cf-string text))
                 (let ((run (msg (msg (objc-class "NSAttributedString") "alloc" :pointer)
                                 "initWithString:attributes:" :pointer string
                                 :pointer (msg (objc-class "NSDictionary")
                                               "dictionaryWithObjects:forKeys:count:"
                                               :pointer objects :pointer keys :unsigned-long 2
                                               :pointer)
                                 :pointer)))
                   (msg title "appendAttributedString:" :pointer run :void)
                   (msg run "release" :void)))))
    title))

(defun show-clock ()
  "Set the menu-bar item's title to CLOCK-RUNS: coloured by CLOCK-TITLE when
*CLOCK-COLORS* is set, else plain in the menu bar's colour."
  (twigwm-macos-apps::with-pool
    (let ((button (msg *item* "button" :pointer)))
      (if *clock-colors*
          (msg button "setAttributedTitle:" :pointer (clock-title) :void)
          (twigwm-macos-apps::with-cf (title (twigwm-macos-apps::cf-string
                                              (format nil "~{~a~}" (mapcar #'car (clock-runs)))))
            (msg button "setTitle:" :pointer title :void))))))

(cffi:defcallback choose :void ((self :pointer) (command :pointer) (item :pointer))
                  (declare (ignore self command))
                  (push (msg item "tag" :long) *chosen*))

(cffi:defcallback tick :void ((timer :pointer) (info :pointer))
                  (declare (ignore timer info))
                  (ignore-errors (show-clock)))

(defun target ()
  "A new TwigWMMenuTarget, the class whose choose: method every menu item sends;
the class is registered on first use."
  (let ((class (objc-class "TwigWMMenuTarget")))
    (when (cffi:null-pointer-p class)
      (setf class (cffi:foreign-funcall "objc_allocateClassPair" :pointer (objc-class "NSObject")
                                        :string "TwigWMMenuTarget" :size 0 :pointer))
      (cffi:foreign-funcall "class_addMethod" :pointer class
                            :pointer (twigwm-macos-apps::selector "choose:")
                            :pointer (cffi:callback choose) :string "v@:@" :unsigned-char)
      (cffi:foreign-funcall "objc_registerClassPair" :pointer class :void))
    (msg class "new" :pointer)))

(defun menu-item (target tag item)
  "An autoreleased NSMenuItem for ITEM, a SHOW plist, sending choose: with TAG to TARGET."
  (twigwm-macos-apps::with-cf (title (twigwm-macos-apps::cf-string (getf item :title)))
    (twigwm-macos-apps::with-cf (key (twigwm-macos-apps::cf-string (or (getf item :key) "")))
      (let ((menu-item (msg (msg (objc-class "NSMenuItem") "alloc" :pointer)
                            "initWithTitle:action:keyEquivalent:" :pointer title
                            :pointer (twigwm-macos-apps::selector "choose:") :pointer key :pointer)))
        (msg menu-item "setTarget:" :pointer target :void)
        (msg menu-item "setTag:" :long tag :void)
        (msg menu-item "setKeyEquivalentModifierMask:" :unsigned-long (getf item :modifiers 0) :void)
        (msg menu-item "autorelease" :pointer)))))

(defun common-modes ()
  "kCFRunLoopCommonModes, which also covers AppKit's menu-tracking mode."
  (cffi:mem-ref (cffi:foreign-symbol-pointer "kCFRunLoopCommonModes") :pointer))

(defun show (items)
  "Put the clock in the menu bar with a menu of ITEMS, without a Dock icon. Each
item is :SEPARATOR or a plist (:TITLE :KEY :MODIFIERS :ACTION): KEY is the key
equivalent shown at the right, MODIFIERS its NSEventModifierFlags, the same bits
as CGEventFlags, and ACTION the function PUMP calls once the item is chosen.
Event-tap sources must be in COMMON-MODES to keep running while a menu is open."
  (with-appkit
    (twigwm-macos-apps::with-pool
      (let ((app (msg (objc-class "NSApplication") "sharedApplication" :pointer))
            (menu (msg (objc-class "NSMenu") "new" :pointer))
            (target (target))
            (run-loop (cffi:foreign-funcall "CFRunLoopGetCurrent" :pointer)))
        (msg app "setActivationPolicy:" :long 1 :unsigned-char)
        (msg app "finishLaunching" :void)
        (cffi:foreign-funcall "CFRunLoopAddCommonMode" :pointer run-loop
                              :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "NSEventTrackingRunLoopMode")
                                                     :pointer)
                              :void)
        (setf *actions* (coerce (loop for item in items
                                      unless (eq item :separator) collect (getf item :action))
                                'vector))
        (loop with tag = -1
              for item in items
              do (msg menu "addItem:" :pointer
                      (if (eq item :separator)
                          (msg (objc-class "NSMenuItem") "separatorItem" :pointer)
                          (menu-item target (incf tag) item))
                      :void))
        (setf *item* (msg (msg (msg (objc-class "NSStatusBar") "systemStatusBar" :pointer)
                               "statusItemWithLength:" :double -1d0 :pointer)
                          "retain" :pointer))
        (msg (msg *item* "button" :pointer) "setFont:" :pointer (clock-font) :void)
        (msg *item* "setMenu:" :pointer menu :void)
        (msg menu "release" :void)
        (show-clock)
        (setf *timer* (cffi:foreign-funcall "CFRunLoopTimerCreate" :pointer (cffi:null-pointer)
                                            :double (+ 1 (ffloor (cffi:foreign-funcall
                                                                  "CFAbsoluteTimeGetCurrent" :double)))
                                            :double 1d0 :unsigned-long 0 :long 0
                                            :pointer (cffi:callback tick) :pointer (cffi:null-pointer)
                                            :pointer))
        (cffi:foreign-funcall "CFRunLoopAddTimer" :pointer run-loop :pointer *timer*
                              :pointer (common-modes) :void)))))

(defun hide ()
  "Remove the menu-bar item SHOW added and stop its clock."
  (when *timer*
    (cffi:foreign-funcall "CFRunLoopTimerInvalidate" :pointer *timer* :void)
    (cffi:foreign-funcall "CFRelease" :pointer *timer* :void)
    (setf *timer* nil))
  (when *item*
    (with-appkit
      (twigwm-macos-apps::with-pool
        (msg (msg (objc-class "NSStatusBar") "systemStatusBar" :pointer) "removeStatusItem:"
             :pointer *item* :void)
        (msg *item* "release" :void)))
    (setf *item* nil)))

(defun pump (mode seconds)
  "Run the run loop in MODE for up to SECONDS while waiting for one AppKit event,
such as a click on the menu-bar item, and handle it; then run the actions of
items chosen meanwhile, once their menu has closed."
  (with-appkit
    (twigwm-macos-apps::with-pool
      (let* ((app (msg (objc-class "NSApplication") "sharedApplication" :pointer))
             (event (msg app "nextEventMatchingMask:untilDate:inMode:dequeue:"
                         :uint64 #xffffffffffffffff
                         :pointer (msg (objc-class "NSDate") "dateWithTimeIntervalSinceNow:"
                                       :double seconds :pointer)
                         :pointer mode :unsigned-char 1 :pointer)))
        (unless (cffi:null-pointer-p event)
          (msg app "sendEvent:" :pointer event :void)))))
  (loop while *chosen*
        do (handler-case (funcall (aref *actions* (pop *chosen*)))
             (error (e) (format *error-output* "Menu action failed: ~a~%" e)))))
