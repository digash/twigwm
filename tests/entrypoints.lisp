;;;; Read and compile both executables without running installers or event taps.
(dolist (file '("../bin/install" "../bin/twigwm"))
  (with-open-file (s (merge-pathnames file *load-truename*))
    (read-line s) ; interpreter line
    (loop for form = (read s nil :end) until (eq form :end) do
      (multiple-value-bind (function warnings failure)
          (compile nil `(lambda () ,form))
        (declare (ignore function warnings))
        (assert (not failure))))))
(format t "ENTRYPOINTS_TESTS_COMPLETE~%")
