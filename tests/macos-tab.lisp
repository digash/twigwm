;;;; Run with: sbcl --script tests/macos-tab.lisp (no macOS frameworks required).
(defpackage :twigwm-macos-input (:use :cl))
(load (merge-pathnames "../src/macos-tab.lisp" *load-truename*))
(in-package :twigwm-macos-input)

;; Local Option-Tab replaces the already-delivered Option with Command.
(let ((*tab-switch* nil))
  (assert (equal (multiple-value-list (tab-switch-step 48 10 #x80020 t 123))
                 '(t nil ((58 12 0) (55 12 #x100008) (48 10 #x100008)))))
  (assert (equal (multiple-value-list (tab-switch-step 48 11 #x80020))
                 '(t nil ((48 11 #x100008)))))
  ;; Hold Shift and cycle backwards, including key repeat.
  (assert (equal (nth-value 2 (tab-switch-step 56 12 #xa0022))
                 '((56 12 #x12000a))))
  (dotimes (i 2)
    (assert (equal (nth-value 2 (tab-switch-step 48 10 #xa0022))
                   '((48 10 #x12000a)))))
  (tab-switch-step 48 11 #xa0022)
  (assert (equal (nth-value 2 (tab-switch-step 58 12 #x20002))
                 '((55 12 #x20002))))
  (assert (null *tab-switch*)))

;; Command-Tab goes directly to the original remote PID as Alt-Tab.
(let ((*tab-switch* nil))
  (assert (equal (multiple-value-list (tab-switch-step 48 10 #x120012 t 123))
                 '(t 123 ((54 12 #x20002) (58 12 #xa0022) (48 10 #xa0022)))))
  (assert (equal (multiple-value-list (tab-switch-step 48 11 #x100010 t 999))
                 '(t 123 ((48 11 #x80020)))))
  (assert (equal (multiple-value-list (tab-switch-step 54 12 0))
                 '(t 123 ((58 12 0)))))
  (assert (null *tab-switch*)))

;; Releasing one side does not finish while the other is still held.
;; A late Tab-up still goes to the original target after modifier release.
(let ((*tab-switch* nil))
  (assert (equal (nth-value 2 (tab-switch-step 48 10 #x100018 t 123))
                 '((55 12 0) (54 12 0) (58 12 #x80020) (48 10 #x80020))))
  (assert (equal (multiple-value-list (tab-switch-step 55 12 #x100010))
                 '(t 123 nil)))
  (assert (equal (nth-value 2 (tab-switch-step 54 12 0)) '((58 12 0))))
  (assert *tab-switch*)
  (assert (not (tab-switch-step 0 10 0))) ; unrelated typing is no longer captured
  (assert (equal (multiple-value-list (tab-switch-step 48 11 0))
                 '(t 123 ((48 11 0)))))
  (assert (null *tab-switch*)))

;; Right Option, Escape cancellation, and aggregate-only modifier flags.
(dolist (flags '(#x80040 #x80000))
  (let ((*tab-switch* nil))
    (assert (equal (nth-value 2 (tab-switch-step 48 10 flags))
                   `((,(if (= flags #x80040) 61 58) 12 0)
                     (55 12 #x100008) (48 10 #x100008))))
    (tab-switch-step 48 11 flags)
    (assert (equal (nth-value 2 (tab-switch-step 53 10 flags))
                   '((53 10 #x100008))))
    (tab-switch-step 53 11 flags)
    (tab-switch-step 61 12 0)
    (assert (null *tab-switch*))))

;; No global modifier swap: normal shortcuts and extra modifiers are untouched.
(let ((*tab-switch* nil))
  (dolist (flags '(0 #x20000 #x40000 #x180000 #x1a0000 #xc0000 #x140000))
    (assert (not (tab-switch-step 48 10 flags t 123))))
  (assert (not (tab-switch-step 48 10 #x100008 nil 123)))
  (assert (not (tab-switch-step 18 10 #x100008 t 123)))
  (assert (not (tab-switch-step 48 11 #x80020 t 123)))
  (assert (null *tab-switch*)))
(format t "PASS: local/remote Tab swap, Shift/repeat, modifier sides/releases, pinned target, late Tab-up, Escape, and unrelated shortcuts.~%")
