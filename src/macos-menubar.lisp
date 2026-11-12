;;;; Menu-bar status item: a clock title and a menu of actions with their shortcuts.
;;;; Copyright (c) 2026 TwigWM contributors. MIT License.
(defpackage :twigwm-macos-menubar
  (:use :cl)
  (:export #:show #:hide #:pump #:clock #:key-equivalent #:*clock-format*))
(in-package :twigwm-macos-menubar)

(defvar *clock-format* "%a %H:%M:%S"
  "strftime format of the menu-bar clock.")

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

(defun clock (&optional (format *clock-format*))
  "The local time as strftime FORMAT, in the C locale this process keeps."
  (cffi:with-foreign-objects ((now :long) (tm :char 64) (text :char 64))
    (setf (cffi:mem-ref now :long)
          (cffi:foreign-funcall "time" :pointer (cffi:null-pointer) :long))
    (cffi:foreign-funcall "localtime_r" :pointer now :pointer tm :pointer)
    (cffi:foreign-funcall "strftime" :pointer text :size 64 :string format :pointer tm :size)
    (cffi:foreign-string-to-lisp text)))

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

(defun show-clock ()
  "Set the menu-bar item's title to CLOCK."
  (twigwm-macos-apps::with-pool
    (twigwm-macos-apps::with-cf (title (twigwm-macos-apps::cf-string (clock)))
      (msg (msg *item* "button" :pointer) "setTitle:" :pointer title :void))))

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
        (msg (msg *item* "button" :pointer) "setFont:" :pointer
             (msg (objc-class "NSFont") "monospacedSystemFontOfSize:weight:"
                  :double (msg (msg (objc-class "NSFont") "menuBarFontOfSize:" :double 0d0 :pointer)
                               "pointSize" :double)
                  :double 0d0 :pointer)
             :void)
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
