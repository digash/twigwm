;;;; Copy to ~/.config/twigwm/config.lisp and customize.
(in-package :twigwm-apps)
(setf *apps*
      (list (make-app :name "terminal" :n 1 :mac "com.apple.Terminal" :mac-region :left)
            (make-app :name "browser" :n 2 :mac "com.apple.Safari" :mac-region :right)
            (make-app :name "editor" :n 3 :mac "org.gnu.Emacs" :mac-region :right)
            (make-app :name "remote" :n nil :mac "com.microsoft.rdc.macos"
                      :mac-passthrough t :mac-region :right)))
(in-package :twigwm-macos-apps)
;; :primary is the first display returned by SCREENS. Use its UUID for a
;; specific monitor; nothing identifying a physical monitor belongs in Git.
(setf *displays* '((:main :primary 0))
      *regions* '((:left :main 0 0 1/3 1) (:right :main 1/3 0 1 1)))
;; Optional remote routing: saved-device names are local user configuration.
;; (setf twigwm-macos-input::*number-device* "desktop"
;;       twigwm-macos-input::*zero-device* "secondary")
