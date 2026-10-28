(defparameter *keys*
  (list
   ;;; window / app switching
   (make-key
    :action :host-escape
    :doc "Return to the local host. StumpWM's s-Escape returns to the last local window.
On macOS ⌘-Escape returns to the previous individual Mac window; from a
remote-session app it returns to the last local one, never another remote window.
From native Mac apps, ⌘-1 through ⌘-9 open the saved desktop device and send
Super-number; ⌘-0 only selects the secondary device. Inside remote-session
apps those keys pass through unchanged, so a second ⌘-0 reaches the session.
The platform slots are deliberately NIL.")

   (make-key
    :action :remote-next-window
    :doc "Alt-Tab shows the global MRU window list and selects on Alt release,
as on Windows. In a remote session Mac Command-Tab arrives as s-Tab."
    :stump "M-Tab" :cmd "all-windowlist-cycle-next %m%n %c %s%t")
   (make-key
    :action :remote-previous-window
    :doc "Alt-Shift-Tab cycles the same global MRU list in reverse."
    :stump "M-ISO_Left_Tab" :cmd "all-windowlist-cycle-previous %m%n %c %s%t")

   (make-key
    :action :maximize-window
    :doc "Option-F10 toggles the focused Mac window between its previous frame and
the current display's usable area, retaining title bar and borders. Remote
applications receive the original chord. StumpWM has no binding for this row."
    :mac '(:keycode 109 :modifiers #x80000 :maximize t))

   ;;; local screenshots — Print, M-Print, s-Print, in that order of scope
   (make-key
    :action :screenshot-region
    :doc "Interactive region select. Native macOS opens Screenshot
(Command-Shift-5). Windows App drops Print/F13, so send Command-Shift-5 there.
StumpWM spells this s-percent because Shift-5 produces percent on our US layout.
RDP passes through; desktop maps its otherwise unmapped X keycode 202 to Print."
    :stump '("Print" "s-percent") :cmd "screenshot-select"
    :mac '(:keycode 105 :bundle "com.apple.screenshot.launcher"
           :chord (("com.microsoft.rdc.macos"
                    (55 12 #x100008) (56 12 #x12000a) ; Command, Shift down
                    (23 10 #x12000a) (23 11 #x12000a) ; 5 down/up
                    (56 12 #x100008) (55 12 0)))))    ; Shift, Command up
   (make-key
    :action :screenshot-window
    :doc "The focused window."
    :stump "M-Print" :cmd "screenshot-window")
   (make-key
    :action :screenshot-screen
    :doc "The whole screen."
    :stump "s-Print" :cmd "screenshot-screen")

   ;;; art on the screen, on demand
   (make-key
    :action :art
    :doc "Public-domain paintings on every display until a keypress, via the
separately installed cl-art package. Replaces the `s-l' that ran xlock. Not a
lock on either platform. TwigWM launches SBCL directly on macOS, resolving the art script relative to the user's home without a shell.
Remote sessions receive the original key."
    :stump "s-l" :cmd "exec art"
    :mac '(:keycode 37 :modifiers #x100000
           :argv ("/opt/homebrew/bin/sbcl" "--dynamic-space-size" "4096"
                  "--script" "~/.local/bin/art" "show")))))
