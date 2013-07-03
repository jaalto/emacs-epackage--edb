;; Prevent loading this file. Study the examples.
(error "edb-epackage-examples.el is not a configuration file.")

** Copy from README

Welcome!

        This directory contains EDB, the Emacs Database.
        EDB was written by Michael Ernst <mernst@theory.lcs.mit.edu>,
        and is being maintained by Thien-Thi Nguyen <ttn@gnuvola.org>.

        EDB homepage: <http://www.gnuvola.org/software/edb/>

Usage

        (require 'database)

        ;; Install location:
        ;; (concat (file-name-directory (locate-libray "database")) "/..")

        Then you can do Dired in subdirs `examples' and `skram', move
        point to various data and .edb files, and use the command:

        (defun my-dired-edb-interact ()
          (interactive)
          (let ((filename (dired-get-filename)))
            (if (string-match "[.]edb$" filename)
                (edb-interact filename nil)
              (db-find-file filename))))

        Full details are in the documentation, highly recommended
        for both end users and programmers.


Reporting Bugs

        If you find a problem with EDB that is not explained in the
        documentation, please send a bug report to <ttn@gnuvola.org>.
        Include EDB version and as concise a test case as possible
        (that reproduces the problem).


;; End of file
