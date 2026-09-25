(defparameter *keys*
  (list
   ;;; window / app switching
   (make-key
    :action :host-escape
    :doc "Return to the local host. StumpWM's s-Escape returns to the last local window.
On macOS ⌘-Escape returns to the previous individual Mac window; from a
remote-session app it returns to the last local one, never another remote window.
From native Mac apps, ⌘-1 through ⌘-9 open the saved desktop device and send
Super-number; ⌘-0 does the same with the secondary device. Inside remote-session
apps those keys pass through unchanged.
The platform slots are deliberately NIL."
    :ahk '("#Escape" . "return"))

   (make-key
    :action :remote-next-window
    :doc "Alt-Tab shows the global MRU window list and selects on Alt release,
as on Windows. With *swap-tab-modifiers* enabled, Mac Command-Tab sends Alt-Tab
to remote apps and Option-Tab opens the local app switcher."
    :stump "M-Tab" :cmd "all-windowlist-cycle-next %m%n %c %s%t")
   (make-key
    :action :remote-previous-window
    :doc "Alt-Shift-Tab cycles the same global MRU list in reverse."
    :stump "M-ISO_Left_Tab" :cmd "all-windowlist-cycle-previous %m%n %c %s%t")

   ;;; clipboard — Insert-based, because that is what X apps speak natively
   (make-key
    :action :copy
    :doc "Copy. Ctrl+Insert is native in X apps, so StumpWM binds nothing; AHK
reclaims Win+C from the Copilot app and sends Ctrl+Insert. Mac apps use native ⌘-C."
    :ahk '("#c" . "Send \"^{Insert}\""))
   (make-key
    :action :paste
    :doc "Paste, the Shift+Insert counterpart of :copy. StumpWM binds S-Insert in
*input-map* only (input-yank-clipboard, see stumpwm/config). Mac apps use native ⌘-V."
    :ahk '("#v" . "Send \"+{Insert}\""))

   ;;; local screenshots — Print, M-Print, s-Print, in that order of scope
   (make-key
    :action :screenshot-region
    :doc "Interactive region select. Windows has no bare-Print binding, so AHK
maps it to the Snip & Sketch overlay (Win+Shift+S). Native macOS opens Screenshot
(Command-Shift-5). Windows App drops Print/F13, so send Command-Shift-5 there.
StumpWM spells this s-percent because Shift-5 produces percent on our US layout.
RDP passes through; desktop maps its otherwise unmapped X keycode 202 to Print."
    :stump '("Print" "s-percent") :cmd "screenshot-select"
    :ahk '("PrintScreen" . "Send \"#+s\"")
    :mac '(:keycode 105 :bundle "com.apple.screenshot.launcher"
           :chord (("com.microsoft.rdc.macos"
                    (55 12 #x100008) (56 12 #x12000a) ; Command, Shift down
                    (23 10 #x12000a) (23 11 #x12000a) ; 5 down/up
                    (56 12 #x100008) (55 12 0)))))    ; Shift, Command up
   (make-key
    :action :screenshot-window
    :doc "The focused window. AHK slot is NIL: Windows already copies the active
window on Alt+Print."
    :stump "M-Print" :cmd "screenshot-window")
   (make-key
    :action :screenshot-screen
    :doc "The whole screen. AHK slot is NIL: Win+Print natively saves a full-screen
shot to Pictures\\Screenshots, which is exactly this."
    :stump "s-Print" :cmd "screenshot-screen")

   ;;; CapsLock — same result, three mechanisms, none of them a keybinding
   (make-key
    :action :caps-as-control
    :doc "CapsLock acts as Control. StumpWM gets it from xkb (twigwm's setxkbmap
ctrl(nocaps)), the Mac from macOS Modifier Keys; only Windows needs a hotkey."
    :ahk '("*CapsLock" . "Ctrl"))

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
