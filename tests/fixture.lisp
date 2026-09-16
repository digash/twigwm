;;;; Synthetic configuration; never reads the user's configuration.
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(in-package :twigwm-apps)
(setf *apps*
      (list (make-app :name "vterm" :n 1 :mac "org.gnu.Emacs" :mac-region :landscape-left
                      :mac-cmd '("emacsclient" "--eval" "(other-frame 1)"))
            (make-app :name "browser" :n 2 :mac "com.google.Chrome")
            (make-app :name "remote" :n 0 :mac "com.microsoft.rdc.macos"
                      :mac-passthrough t :mac-region :laptop)))
(in-package :twigwm-keys)
(load (merge-pathnames "../examples/keys.lisp" *load-truename*))
(in-package :twigwm-macos-input)
(setf *number-device* "desktop" *zero-device* "secondary")
(in-package :twigwm-macos-apps)
(setf *guest-command* '("ssh" "desktop" "window-command")
      *displays* '((:portrait "00000000-0000-0000-0000-000000000001" 31)
                  (:landscape "00000000-0000-0000-0000-000000000002" 31)
                  (:laptop "00000000-0000-0000-0000-000000000003" 0))
      *regions* '((:landscape-left :landscape 0 0 1/3 1)
                  (:landscape-right :landscape 1/3 0 1 1)
                  (:portrait-top :portrait 0 0 1 1/3)
                  (:portrait-main :portrait 0 1/3 1 1)
                  (:laptop :laptop 0 0 1 1)))
