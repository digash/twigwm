;;;; TwigWM — Common Lisp window control.
;;;; Copyright (c) 2026 TwigWM contributors. MIT License.

(defpackage :twigwm-apps
  (:use :cl)
  (:export #:app #:make-app #:app-p
           #:app-name #:app-cmd #:app-instance #:app-class #:app-title
           #:app-n #:app-frame #:app-group #:app-kind #:app-hosts
           #:app-key #:app-fullscreen
           #:app-mac #:app-mac-cmd #:app-mac-passthrough #:app-mac-region
           #:*apps* #:*mac-host* #:host-match-p #:apps-for-host #:wslg-app-p #:wslg-title
           #:win-numbers #:app-for-number
           #:chrome-desktop-apps #:chrome-mac-apps #:chrome-apps #:pwa-crx-id
           #:apps-for-mac #:mac-bundle #:mac-numbers #:mac-passthrough-bundles))

(in-package :twigwm-apps)

(defstruct app
  "One launchable/raisable app. KIND is one of :terminal :browser :emacs :pwa
:native :see. HOSTS is :all, a hostname substring, or a list of substrings.
Host strings match case-insensitive substrings.
For :pwa, TITLE is the chrome .desktop Name (the crx_ instance is resolved at
run time by the StumpWM driver); CMD is derived from TITLE when NIL.
MAC is the bundle identifier of the *native* mac app where one exists — it wins
over the PWA there, since a real app beats a browser tab. For :pwa entries
without MAC the bundle id is derived from the installed crx id
(com.google.Chrome.app.<crx>), so it cannot drift. MAC-CMD is an argv list
run after the app opens — ⌘-N's equivalent of an emacsclient --eval.
MAC-PASSTHROUGH marks apps that must receive ⌘-N themselves instead of having
twigwm eat it: remote sessions and VMs, which run their own N map inside.
MAC-REGION restores the selected Mac window on each shortcut press; NIL disables
placement. Most apps share the portrait display's lower two thirds.
KEY binds a literal key instead of s-N; FULLSCREEN fullscreens what it raises."
  name cmd instance class title
  (n 0) (frame 0) group kind (hosts :all)      ; N NIL: no s-N; KEY or metadata only
  key fullscreen
  mac mac-cmd mac-passthrough (mac-region :main))

(defvar *apps* nil)

;;; ——— queries ———

(defun host-match-p (spec hostname)
  (cond ((eq spec :all) t)
        ((stringp spec) (and (search spec hostname :test #'char-equal) t))
        ((listp spec) (some (lambda (s) (host-match-p s hostname)) spec))
        (t nil)))

(defun apps-for-host (hostname)
  "Registry entries applicable to HOSTNAME, in registry order."
  (remove-if-not (lambda (a) (host-match-p (app-hosts a) hostname)) *apps*))

(defun wslg-app-p (a)
  "True if A actually shows a window under WSLg (excludes native-Linux-only and
see terminals, which are not part of the WSLg app set)."
  (member (app-kind a) '(:terminal :browser :emacs :pwa)))

(defun wslg-title (a)
  "Substring guaranteed to appear in A's Windows-visible (WSLg) title."
  (case (app-kind a)
    (:terminal "VTerm")
    (:browser "Google Chrome")
    (:emacs "Emacs")
    (t (app-title a))))

(defun win-numbers (apps)
  "Sorted distinct non-zero s-N / Win+N numbers present in APPS. Entries with a
NIL number carry no shortcut at all, and drop out here."
  (sort (remove-duplicates (remove 0 (remove-if-not #'integerp (mapcar #'app-n apps))))
        #'<))

(defun app-for-number (apps n)
  "The app N raises: the LAST entry in APPS with number N (matching StumpWM's
last-wins define-key). APPS should already be host-filtered."
  (let ((matches (remove n apps :key #'app-n :test-not #'eql)))
    (car (last matches))))

;;; ——— chrome PWAs: title -> crx id ———
;;; Shared with twigwm-w32 (taskbar renaming) and native macOS app shortcuts:
;;; all three need the same title -> crx mapping defweb-shortcut resolves.

(defun parse-chrome-desktop (f)
  "Return (NAME . CRX-ID) parsed from a chrome-*.desktop file, or NIL."
  (with-open-file (in f :if-does-not-exist nil)
    (when in
      (let (name)
        (loop for line = (read-line in nil) while line do
          (when (uiop:string-prefix-p "Name=" line)
            (setf name (subseq line 5)))
          (when (uiop:string-prefix-p "Exec=" line)
            (let ((i (search "--app-id=" line)))
              (when (and i name)
                (let* ((a (subseq line (+ i 9)))
                       (sp (position #\Space a)))
                  (return (cons name (format nil "crx_~a" (if sp (subseq a 0 sp) a)))))))))))))

(defun chrome-desktop-apps ()
  "Parse ~/.local/share/applications/chrome-*.desktop -> list of (NAME . CRX-ID)."
  (let ((dir (format nil "~a/applications/"
                     (or (uiop:getenv "XDG_DATA_HOME")
                         (format nil "~a/.local/share" (uiop:getenv "HOME"))))))
    (loop for f in (directory (merge-pathnames "chrome-*.desktop" dir))
          for pair = (parse-chrome-desktop f)
          when pair collect pair)))

;;; Discover native PWA identifiers on this machine.
(defun mac-pwa-bundle-id (app-dir)
  "CFBundleIdentifier from APP-DIR/Contents/Info.plist, or NIL.
plutil handles both XML and binary app plists; `defaults read /path/Info' no
longer reads arbitrary plist paths on current macOS."
  (let* ((plist (format nil "~aContents/Info.plist" (namestring app-dir)))
         (out (with-output-to-string (s)
                (ignore-errors
                 (uiop:run-program (list "/usr/bin/plutil" "-extract"
                                         "CFBundleIdentifier" "raw" "-o" "-" plist)
                                   :output s :error-output nil
                                   :ignore-error-status t))))
         (id (string-trim '(#\Space #\Newline #\Return) out)))
    (and (plusp (length id)) id)))

(defun chrome-mac-apps ()
  "Parse ~/Applications/Chrome Apps.localized/*.app -> list of (NAME . CRX-ID),
the mac counterpart of CHROME-DESKTOP-APPS. NAME is the bundle directory name
\(there is no CFBundleDisplayName in these), CRX-ID keeps the \"crx_\" prefix so
both sources are interchangeable.

Sorted by name length so that when chrome has installed a duplicate — \"GitHub\"
and \"GitHub 1\" both exist here — the shortest, i.e. the exact title, wins the
substring match in PWA-CRX-ID."
  (let ((dir (format nil "~a/Applications/Chrome Apps.localized/"
                     (uiop:getenv "HOME"))))
    (sort
     (loop for app in (uiop:subdirectories dir)
           for name = (car (last (pathname-directory app)))
           for id = (mac-pwa-bundle-id app)
           when (and id (uiop:string-prefix-p "com.google.Chrome.app." id))
             collect (cons (if (uiop:string-suffix-p name ".app")
                               (subseq name 0 (- (length name) 4))
                               name)
                           (format nil "crx_~a" (subseq id 22))))
     #'< :key (lambda (p) (length (car p))))))

(defun chrome-apps ()
  "Installed PWAs as (NAME . CRX-ID) for whichever OS this is running on."
  (if (uiop:file-exists-p "/usr/bin/sw_vers")
      (chrome-mac-apps)
      (chrome-desktop-apps)))

(defun pwa-crx-id (title &optional (desktops (chrome-apps)))
  "Crx id (no crx_ prefix) of the installed PWA whose .desktop Name contains
TITLE — the same substring match defweb-shortcut uses. NIL when not installed."
  (let ((hit (find-if (lambda (pair) (search title (car pair) :test #'char-equal))
                      desktops)))
    (and hit (subseq (cdr hit) 4))))          ; strip "crx_"

;;; ——— macOS view of the registry ———

(defvar *mac-host* (machine-instance))
(defun apps-for-mac () (apps-for-host *mac-host*))

(defun mac-bundle (a &optional (desktops (chrome-apps)))
  "A's macOS bundle identifier: the native app when the registry names one, else
the Chrome PWA bundle derived from the installed crx id. NIL when the mac has no
way to open A — a Linux-only app, or a PWA that is not installed here."
  (or (app-mac a)
      (and (eq (app-kind a) :pwa)
           (let ((crx (pwa-crx-id (app-title a) desktops)))
             (and crx (format nil "com.google.Chrome.app.~a" crx))))))

(defun mac-numbers (apps)
  "⌘-N numbers to emit: 1..9 then 0, the physical order of the number row."
  (append (win-numbers apps)
          (when (find 0 apps :key #'app-n :test #'eql) '(0))))

(defun mac-passthrough-bundles (apps)
  "Bundle ids of apps that must see ⌘-N themselves (remote sessions, VMs)."
  (remove nil (mapcar #'app-mac (remove-if-not #'app-mac-passthrough apps))))
