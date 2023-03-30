;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)
(require 'el-mock)

(when (require 'undercover nil t)
  (undercover "*.el"))
(require 'obs-dsl (expand-file-name "obs-dsl.el"))

(ert-deftest obs-dsl-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (macrop 'obs-dsl)))

(provide 'obs-dsl-test)
