
;;;### (autoloads (db-find-file) "edbcore" "edbcore.el"
;;;;;;  (20947 59410 0 0))
;;; Generated autoloads from ../lisp/edbcore.el

(autoload 'db-find-file "edbcore" "\
Read a database from FILENAME; prompts when called interactively.
If the database file doesn't specify a format and the format file can't be
inferred from FILENAME, the user is prompted for it too.
The user is always prompted for the format if prefix argument
PROMPTP is non-nil.
If the database is already read in and PROMPTP is nil, the existing
database buffer is merely selected.
When called non-interactively, argument PROMPTP may be a string, the
name of a format file to use.

\(fn FILENAME &optional PROMPTP)" t nil)

;;;***

;;;### (autoloads (databases-compatible db-merge db-process-two-databases)
;;;;;;  "db-two-dbs" "db-two-dbs.el" (20947 58410
;;;;;;  0 0))
;;; Generated autoloads from ../lisp/db-two-dbs.el

(autoload 'db-process-two-databases "db-two-dbs" "\


\(fn DB1 DB2 LONE1 LONE2 CORR &optional ORDERER)" nil nil)

(autoload 'db-merge "db-two-dbs" "\
Merge two databases chosen with completion among read-in databases.

\(fn)" t nil)

(autoload 'databases-compatible "db-two-dbs" "\
Return t if the database records have the same field names and type,
nil otherwise.

\(fn DB1 DB2)" nil nil)

;;;***

;;;### (autoloads (db-tagged-setup) "db-tagged" "db-tagged.el"
;;;;;;  (20947 58410 0 0))
;;; Generated autoloads from ../lisp/db-tagged.el

(autoload 'db-tagged-setup "db-tagged" "\
Ready the database to read files in tagged format.
Argument FSPECS is a list of tagged-field specifications, one for each field
in a database record.  Each tagged-field specification is a three-element
list of the field name (a symbol), the tag used to identify it in the file
\(a string), and a brief help string.  Instead of a symbol, the tagged-field
name may be a cons of the field name and its type.  To indicate that a field
is never found in the input file (typically because it is computed on the
fly), use nil for its tag.

ATTRS is a sequence of alternating keywords and values specifying overriding
attributes.  Here are the valid attributes and their default values:

 :tag-chars           \"-_A-Za-z\"
 :pre-tag             \"\"
 :pre-tag-regexp      nil
 :pre-tag-output      nil
 :separator           \":\"
 :separator-regexp    nil
 :separator-output    nil
 :continuation        \"	\"
 :continuation-regexp \"[ 	]+\"
 :continuation-output nil
 :pre-parse-thunk     nil
 :pre-write-function  nil
 :post-write-function nil
 :index-function      nil
 :default-field       nil

Note: Do not call `database-set-fieldnames-to-list' if using this function.

\(fn FSPECS &rest ATTRS)" nil nil)

;;;***

;;;### (autoloads (database-sort-interface database-sort) "db-sort"
;;;;;;  "db-sort.el" (20947 58410 0 0))
;;; Generated autoloads from ../lisp/db-sort.el

(autoload 'database-sort "db-sort" "\
Sort and return DB, which is also side-effected.
If DB is nil, use the current database.
SORTER is either a field priorities list or a function which
takes two records as arguments and returns t if r1 < r2.

\(fn DB &optional SORTER HIDDEN-RECORDS-AT-END-P)" nil nil)

(autoload 'database-sort-interface "db-sort" "\


\(fn DB)" nil nil)

;;;***

;;;### (autoloads (db-match db-print-match-pattern db-parse-match-pattern)
;;;;;;  "db-search" "db-search.el" (20947 58410 0
;;;;;;  0))
;;; Generated autoloads from ../lisp/db-search.el

(autoload 'db-parse-match-pattern "db-search" "\


\(fn STRING DS)" nil nil)

(autoload 'db-print-match-pattern "db-search" "\


\(fn PAT DS)" nil nil)

(autoload 'db-match "db-search" "\


\(fn PAT TARG RS)" nil nil)

;;;***

;;;### (autoloads (db-rdb-setup) "db-rdb" "db-rdb.el"
;;;;;;  (20947 58410 0 0))
;;; Generated autoloads from ../lisp/db-rdb.el

(autoload 'db-rdb-setup "db-rdb" "\
Ready the database to read files in RDB format.
Creates database local variables and sets database slots.  Argument
RFSPECS is a list of rdb-field specifications, one for each
field in a database record.  Optional, second argument LOCK-FLAG should
be non-nil to lock the file for synchronized updates.  The locking and
unlocking is done with \"rdblock\" and \"rdbunlock\", which must be
available in the current PATH environment variable.

Each field specification is a three-element list of the field name (a
symbol), the tag used to identify it in the file (a string), and a
brief help string.  Instead of a symbol, the rdb-field name may be a
two-element list of the field name its type.  To indicate that a field
is never found in the input file (typically because it is computed on
the fly), use nil for its tag.

\(fn RFSPECS &optional LOCK-FLAG)" nil nil)

;;;***

;;;### (autoloads (db-find-file) "db-interfa" "db-interfa.el"
;;;;;;  (20947 58410 0 0))
;;; Generated autoloads from ../lisp/db-interfa.el

(autoload 'db-find-file "db-interfa" "\
Read a database from FILENAME; prompts when called interactively.
If the database file doesn't specify a format and the format file can't be
inferred from FILENAME, the user is prompted for it too.
The user is always prompted for the format if prefix argument
PROMPTP is non-nil.
If the database is already read in and PROMPTP is nil, the existing
database buffer is merely selected.
When called non-interactively, argument PROMPTP may be a string, the
name of a format file to use.

\(fn FILENAME &optional PROMPTP)" t nil)

;;;***
(provide 'edb-epackage-0loaddefs)
