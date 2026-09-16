;;;; TwigWM — Common Lisp window control.
;;;; Copyright (c) 2026 TwigWM contributors. MIT License.

;;;; WSLg window control via CLX, driven by the shared :twigwm-apps registry.
;;;;
;;;; WSLg runs Weston rootless and REPARENTS each X toplevel into a frame window,
;;;; so the app window (the one carrying WM_CLASS) is a grandchild of root: size
;;;; goes on the app window, position on the frame. A window left in
;;;; _NET_WM_STATE_MAXIMIZED_*/FULLSCREEN ignores resize, so clear that first.

(require :clx)

(defpackage :twigwm-w32
  (:use :cl :twigwm-apps)
  (:export #:arrange #:rename-windows #:emit-ahk #:watch #:start-app #:launch-cmd))

(in-package :twigwm-w32)

(defvar *frame-names* '("VTerm"))

;;; ——— connection ———

(defmacro with-display ((d root) &body body)
  `(let* ((,d (xlib:open-default-display)))
     (unwind-protect
          (let ((,root (xlib:screen-root (first (xlib:display-roots ,d)))))
            ,@body)
       (ignore-errors (xlib:close-display ,d)))))

(defun root-size (root)
  (values (xlib:drawable-width root) (xlib:drawable-height root)))

;;; ——— enumeration ———

(defstruct (win (:constructor make-win (id frame instance class name)))
  ;; INSTANCE is WM_CLASS's res-name, CLASS its res-class. Both matter: chrome app
  ;; (PWA) windows carry crx_<id> as the instance while the class stays
  ;; "Google-chrome", so instance is the only way to tell one PWA from another.
  id frame instance class name)

(defmacro ignoring-x-errors (&body body)
  "BODY, yielding NIL if the window went away mid-request. Chrome creates and
drops helper windows constantly, and an unhandled error would kill the arranger."
  `(handler-case (progn ,@body)
     (xlib:window-error () nil)
     (xlib:drawable-error () nil)
     (xlib:match-error () nil)))

(defun app-wins (root)
  "Managed app windows: grandchildren carrying WM_CLASS under viewable frames.
Returns WIN structs pairing the app window ID with its reparenting FRAME."
  (loop for frame in (or (ignoring-x-errors (xlib:query-tree root)) '())
        when (eq (ignoring-x-errors (xlib:window-map-state frame)) :viewable)
          append (loop for w in (or (ignoring-x-errors (xlib:query-tree frame)) '())
                       for (instance class) = (multiple-value-list
                                               (ignoring-x-errors (xlib:get-wm-class w)))
                       when class
                         collect (make-win w frame instance class
                                           (ignoring-x-errors (xlib:wm-name w))))))

(defun win-matches-p (w needle)
  "True if NEEDLE (substring) is in W's instance, class or name."
  (flet ((has (s) (and s (search needle s :test #'char-equal))))
    (or (has (win-instance w)) (has (win-class w)) (has (win-name w)))))

(defun find-wins (wins needle)
  (remove-if-not (lambda (w) (win-matches-p w needle)) wins))

;;; ——— primitives (verified live) ———

(defun undecorate (w)
  "Drop window decorations, unless already undecorated (avoid redundant churn)."
  (let ((cur (ignore-errors (xlib:get-property (win-id w) :_MOTIF_WM_HINTS
                                               :result-type 'list))))
    (unless (equal cur '(2 0 0 0 0))
      (xlib:change-property (win-id w) :_MOTIF_WM_HINTS '(2 0 0 0 0)
                            :_MOTIF_WM_HINTS 32))))

(defun maximized-p (w)
  "True if W currently carries any maximized/fullscreen state."
  (plusp (length (or (ignore-errors (xlib:get-property (win-id w) :_NET_WM_STATE
                                                       :result-type 'list))
                     '()))))

(defun clear-max-state (d root w)
  "Remove maximized/fullscreen state (EWMH client message) so resize is honored."
  (flet ((rm (a &optional b)
           (xlib:send-event root :client-message
                            '(:substructure-redirect :substructure-notify)
                            :window (win-id w) :type :_NET_WM_STATE :format 32
                            :data (vector 0 (xlib:intern-atom d a)
                                          (if b (xlib:intern-atom d b) 0) 1 0))))
    (rm :_NET_WM_STATE_MAXIMIZED_HORZ :_NET_WM_STATE_MAXIMIZED_VERT)
    (rm :_NET_WM_STATE_FULLSCREEN)))

(defparameter *requested-sizes* (make-hash-table)
  "Window id -> the last (WIDTH . HEIGHT) we asked that window to be.")

(defun size-satisfied-p (w width height)
  "True if W is already the target size, or was already asked for it. The second
case matters: Emacs frames quantize to character cells and settle short of any
request, and re-issuing a resize shifts the window sideways (Weston recomputes the
frame), so an exact-match-only test would drift them forever."
  (let ((key (xlib:window-id (win-id w)))
        (target (cons width height)))
    (or (and (= (xlib:drawable-width (win-id w)) width)
             (= (xlib:drawable-height (win-id w)) height))
        (equal (gethash key *requested-sizes*) target))))

(defun note-requested-size (w width height)
  (setf (gethash (xlib:window-id (win-id w)) *requested-sizes*)
        (cons width height)))

(defun resize (d root w width height)
  "Undecorate W, clear its maximized state, and size it to WIDTH×HEIGHT.
Deliberately does NOT touch position: under RAIL each toplevel is its own Windows
window, so Windows owns placement, and the frame's coordinates are relative to a
non-root parent — setting them repeatedly marched windows off the screen."
  (ignoring-x-errors (undecorate w))
  (ignoring-x-errors (when (maximized-p w) (clear-max-state d root w)))
  (ignoring-x-errors
    (unless (size-satisfied-p w width height)
      (setf (xlib:drawable-width (win-id w)) width
            (xlib:drawable-height (win-id w)) height)
      (note-requested-size w width height))))

(defun place (d root w x y width height)
  "RESIZE plus frame position — only for the tiling multi-monitor layouts; see the
drift note in RESIZE before using it on the single-display path."
  (resize d root w width height)
  (setf (xlib:drawable-x (win-frame w)) x
        (xlib:drawable-y (win-frame w)) y))

;;; ——— renaming (taskbar icons) ———

;;; CHROME-DESKTOP-APPS (title -> crx id) lives in the shared :twigwm-apps registry.

(defun rename-win (w instance class)
  (ignoring-x-errors
    (xlib:set-wm-class (win-id w) instance class)
    (setf (xlib:wm-name (win-id w)) class)
    (xlib:unmap-window (win-id w))
    (xlib:map-window (win-id w))))

(defun rename-windows (d root)
  "Rename chrome-app windows (crx_<id> -> .desktop Name) and Emacs sub-windows,
for correct WSLg taskbar icons."
  (let ((wins (app-wins root)))
    (dolist (pair (chrome-desktop-apps))
      (dolist (w (find-wins wins (cdr pair)))       ; match crx_ instance
        (rename-win w (car pair) (car pair))))
    ;; Emacs named sub-frames (VTerm etc.) already carry their name as WM_NAME;
    ;; give them a distinct class so the taskbar separates them.
    (dolist (name *frame-names*)
      (dolist (w (find-wins (app-wins root) name))
        (when (and (win-name w) (string= (win-name w) name)
                   (not (string= (or (win-class w) "") name)))
          (rename-win w name name)))))
  (xlib:display-finish-output d))

;;; ——— native Windows apps (layout-2/3): an AHK subprocess, not X ———

(defun w32-move (x y w h selector)
  (let* ((user (uiop:getenv "USER"))
         (ahk (format nil "/c/Users/~a/AppData/Local/Programs/AutoHotkey/v2/AutoHotkey64.exe" user))
         (script (format nil "C:\\Users\\~a\\AppData\\Local\\AutoHotkey\\w32-move-window.ahk" user)))
    (ignore-errors
     (uiop:run-program (list ahk script (princ-to-string x) (princ-to-string y)
                             (princ-to-string w) (princ-to-string h) selector)
                       :ignore-error-status t))))

;;; ——— layouts ———

(defun maximize-all (d root)
  "Fill the display with every managed app window (single-display layout)."
  (multiple-value-bind (w h) (root-size root)
    ;; Clear state for all windows first, then size: a still-maximized window
    ;; ignores resize, so the compositor needs to settle in between.
    (let ((wins (app-wins root)))
      (dolist (win wins) (undecorate win) (clear-max-state d root win))
      (xlib:display-finish-output d)
      (dolist (win wins) (resize d root win w h))
      (xlib:display-finish-output d))))

(defun place-needle (d root wins needle x y w h)
  (dolist (win (find-wins wins needle)) (place d root win x y w h)))

(defvar *layout-function* nil)

(defun arrange* (d root monitors)
  "Rename windows for taskbar icons, then lay out for MONITORS (1/2/3, else
maximize all). Operates on an already-open display."
  (rename-windows d root)
  (if *layout-function* (funcall *layout-function* d root monitors) (maximize-all d root)))

(defun arrange (&optional (monitors 1))
  (with-display (d root) (arrange* d root monitors)))

;;; ——— AHK Win+N generation (from the registry, so it never drifts from s-N) ———

(defparameter *ahk-begin* "; >>> Win+N generated from the twigwm-apps registry >>>")
(defparameter *ahk-end*   "; <<< Win+N generated <<<")
(defparameter *ahk-keys-begin* "; >>> keys generated from the twigwm-keys table >>>")
(defparameter *ahk-keys-end*   "; <<< keys generated <<<")

(defun default-ahk-path ()
  (format nil "/c/Users/~a/AppData/Local/AutoHotkey/w32-keys.ahk"
          (uiop:getenv "USER")))

(defun ahk-string (text)
  "TEXT as an AutoHotkey v2 double-quoted literal (inner double quotes doubled)."
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for c across text
          do (when (char= c #\") (write-char #\" s))
             (write-char c s))
    (write-char #\" s)))

(defun launch-cmd (a)
  "Shell command that starts A inside WSL. PWAs go through xdg-open-desktop (the
same path defweb-shortcut uses in StumpWM); everything else uses its :cmd."
  (if (eq (app-kind a) :pwa)
      (format nil "xdg-open-desktop '~a'" (app-title a))
      (app-cmd a)))

(defun raise-app (a)
  "Focus+raise an existing window of A, or NIL if none is open."
  (with-display (d root)
    (let ((w (first (find-wins (app-wins root) (wslg-title a)))))
      (when w
        (ignoring-x-errors
          (xlib:circulate-window-up (win-frame w))
          (setf (xlib:window-priority (win-id w)) :above)
          (xlib:set-input-focus d (win-id w) :parent))
        (xlib:display-finish-output d)
        t))))

(defun await-window (a &optional (monitors 1) (tries 20))
  "Arrange repeatedly until A's window shows up (or TRIES seconds pass). ARRANGE
also does the renaming that makes a freshly-launched PWA matchable."
  (loop repeat tries
        do (ignore-errors (arrange monitors))
           (when (with-display (d root)
                   (progn d (find-wins (app-wins root) (wslg-title a))))
             (return t))
           (sleep 1)))

(defun start-app (name &optional (hostname (machine-instance)) (monitors 1))
  "Run-or-raise the registry app called NAME, then arrange. Raises an existing
window if there is one, else launches and waits for it to appear. Returns :raised,
the command run, or NIL if NAME is unknown.

Arranging happens HERE, on the s-N / Win+N keypress, rather than from a background
event loop: windows are only ever wrong right after one appears or is raised, and
doing it on the keypress is predictable instead of racing the compositor."
  (let ((a (find name (apps-for-host hostname) :key #'app-name :test #'string-equal)))
    (when a
      (if (raise-app a)
          (progn (ignore-errors (arrange monitors)) :raised)
          (let ((cmd (launch-cmd a)))
            (when cmd
              ;; :directory matters — Win+N arrives here from AutoHotkey through
              ;; wsl.exe, which hands us AHK's Windows working directory
              ;; (/c/Users/<user>/AppData/Local/AutoHotkey). emacsclient passes its
              ;; cwd to the daemon as the new frame's default-directory, so without
              ;; this s-1/s-3 opened vterm and Emacs there instead of at home.
              (uiop:launch-program (list "sh" "-c" cmd)
                                   :directory (uiop:getenv "HOME"))
              (await-window a monitors)
              cmd))))))

(defun win+n-lines (hostname)
  "The #N::ActivateOrStart lines for HOSTNAME — the Windows-side run-or-raise."
  (let* ((apps (apps-for-host hostname))
         (wapps (remove-if-not #'wslg-app-p apps)))
    (with-output-to-string (s)
      (dolist (n (win-numbers apps))
        (let ((a (app-for-number wapps n)))
          (when a
            ;; Emit the registry NAME, not the command: names are bare tokens, so
            ;; nothing needs quoting across Windows -> wsl.exe -> bash (some
            ;; commands contain double quotes). `twigwm start NAME` resolves it.
            (format s "#~d::ActivateOrStart(~a, ~a)~%"
                    n (ahk-string (wslg-title a))
                    (ahk-string (app-name a)))))))))

(defun key-lines ()
  "The synced-key hotkeys from the shared twigwm-keys table — the Windows spelling of
the same actions used by StumpWM and native macOS. Rows with a NIL AHK slot are absent
on purpose (Windows already does them natively); see the table for which."
  (with-output-to-string (s)
    (dolist (k (twigwm-keys:ahk-keys))
      (destructuring-bind (hotkey . action) (twigwm-keys:key-ahk k)
        (format s "~a::~a    ; ~(~a~)~%" hotkey action (twigwm-keys:key-action k))))))

(defun replace-block (lines begin end body)
  "LINES with everything between the BEGIN and END marker lines replaced by BODY.
NIL (with a warning) when the markers are missing, so a partial rewrite of one
block never truncates the file."
  (let ((b (position begin lines :test #'string=))
        (e (position end lines :test #'string=)))
    (if (and b e (< b e))
        (append (subseq lines 0 (1+ b))
                (list (string-right-trim '(#\Newline) body))
                (nthcdr e lines))
        (warn "emit-ahk: markers not found: ~a" begin))))

(defun emit-ahk (&key (hostname (machine-instance)) (path (default-ahk-path)))
  "Rewrite both generated blocks in PATH — Win+N from the registry, the synced
hotkeys from the twigwm-keys table. Idempotent; leaves a block alone (with a
warning) if its markers are absent."
  (let* ((lines (uiop:read-file-lines path))
         (lines (or (replace-block lines *ahk-begin* *ahk-end* (win+n-lines hostname))
                    lines))
         (lines (or (replace-block lines *ahk-keys-begin* *ahk-keys-end* (key-lines))
                    lines)))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (dolist (l lines) (write-line l s)))
    path))

;;; ——— watch: event-driven re-arrange (RandR) + new-window auto-arrange ———

(defun drain-events (d)
  "Discard any queued events without blocking (burst coalescing)."
  (loop while (and (xlib:event-listen d 0) (plusp (xlib:event-listen d 0)))
        do (xlib:process-event d :timeout 0
                              :handler (lambda (&rest event) (declare (ignore event)) t))))

(defun watch (&optional (monitors 1))
  "Arrange what is open, then re-arrange on RandR screen-change and on new windows
mapping (debounced). Blocks forever."
  (with-display (d root)
    (setf (xlib:window-event-mask root) '(:substructure-notify))
    (ignore-errors (xlib:rr-select-input root '(:screen-change-notify-mask)))
    (arrange* d root monitors)
    (loop
      (let ((hit nil))
        ;; Not :configure-notify — our own resizes emit those, and the arranger
        ;; would re-trigger itself in a loop.
        (xlib:process-event d :force-output-p nil :discard-p t
          :handler (lambda (&key event-key &allow-other-keys)
                     (when (member event-key '(:rr-screen-change-notify :map-notify))
                       (setf hit t))
                     t))
        (when hit
          (sleep 1)             ; let the burst / new geometry settle
          (ignoring-x-errors (drain-events d))
          (handler-case (arrange* d root monitors)
            (error (e) (format *error-output* "~&arrange: ~a~%" e))))))))
