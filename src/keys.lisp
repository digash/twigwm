;;;; TwigWM — Common Lisp window control.
;;;; Copyright (c) 2026 TwigWM contributors. MIT License.

(defpackage :twigwm-keys
  (:use :cl)
  (:export #:key #:make-key #:key-p
           #:key-action #:key-doc #:key-stump #:key-cmd #:key-ahk #:key-mac
           #:*keys* #:stump-keys #:ahk-keys #:mac-keys))

(in-package :twigwm-keys)

(defstruct key
  "One action, spelled per platform. Slots:
  ACTION  keyword naming the action (the row's identity)
  DOC     what it does, plus why a platform slot is NIL
  STUMP   StumpWM key spec or list of aliases for `kbd', e.g. \"s-Print\"
  CMD     StumpWM command string STUMP runs
  AHK     (HOTKEY . ACTION) AutoHotkey v2 strings, e.g. (\"#c\" . \"Send \\\"^{Insert}\\\"\")
  MAC     Native input plist: :keycode CODE :modifiers MASK (default zero),
          :bundle ID or :argv (PROGRAM ARG...), with ~/ resolved in Lisp.
          :chord ((BUNDLE (CODE TYPE FLAGS)...)) sends a chord to a named app.
          Other modifiers/remotes pass through."
  action doc stump cmd ahk mac)

(defvar *keys* nil)

;;; ——— per-platform views ———

(defun stump-keys ()
  "Rows StumpWM binds (those with both a key spec and a command)."
  (remove-if-not (lambda (k) (and (key-stump k) (key-cmd k))) *keys*))

(defun ahk-keys ()
  "Rows AutoHotkey binds."
  (remove-if-not #'key-ahk *keys*))

(defun mac-keys ()
  "Rows with native Mac bindings."
  (remove-if-not #'key-mac *keys*))
