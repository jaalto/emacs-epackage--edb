(dolist (file
         '("lisp/connection.el"
	   "lisp/database.el"
	   "lisp/db-file-io.el"
	   "lisp/db-format.el"
	   "lisp/db-interfa.el"
	   "lisp/db-isbn.el"
	   "lisp/db-lemacs.el"
	   "lisp/db-nosetf.el"
	   "lisp/db-oldnames.el"
	   "lisp/db-rdb.el"
	   "lisp/db-rep.el"
	   "lisp/db-search.el"
	   "lisp/db-sort.el"
	   "lisp/db-summary.el"
	   "lisp/db-tagged.el"
	   "lisp/db-two-dbs.el"
	   "lisp/db-types.el"
	   "lisp/db-util.el"
	   "lisp/edb-1int-to-single.el"
	   "lisp/edb-meta.el"
	   "lisp/edb-t-human-names.el"
	   "lisp/edb-t-places-usuk.el"
	   "lisp/edb-t-timedate1.el"
	   "lisp/edbcore.el"
	   "lisp/state.el"
	   "lisp/system.el"
	   "skram/make-hacksup.el"
	   "skram/make-skram.el"))
  (if (and (boundp 'verbose)
	   verbose)
      (message "Byte Compile %s" file))
  (if (file-exists-p file)
      (byte-compile-file file)
    (message "** Byte compile error. Not found: %s" file)))
