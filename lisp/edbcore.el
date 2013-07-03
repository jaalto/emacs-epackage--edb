;;; system.el

;; Copyright (C) 2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'easymenu)

(unless (fboundp 'with-selected-window)
  ;; snarfed 2005-01-18 from GNU Emacs subr.el (CVS revision 1.432);
  ;; modified to call `select-window' with only one arg
  (defmacro with-selected-window (window &rest body)
    "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.
This does not alter the buffer list ordering.
This function saves and restores the selected window, as well as
the selected window in each frame.  If the previously selected
window of some frame is no longer live at the end of BODY, that
frame's selected window is left alone.  If the selected window is
no longer live, then whatever window is selected at the end of
BODY remains selected.
See also `with-temp-buffer'."
    (declare (indent 1) (debug t))
    ;; Most of this code is a copy of save-selected-window.
    `(let ((save-selected-window-window (selected-window))
           ;; It is necessary to save all of these, because calling
           ;; select-window changes frame-selected-window for whatever
           ;; frame that window is in.
           (save-selected-window-alist
            (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
                    (frame-list))))
       (unwind-protect
           (progn (select-window ,window)
                  ,@body)
         (dolist (elt save-selected-window-alist)
           (and (frame-live-p (car elt))
                (window-live-p (cadr elt))
                (set-frame-selected-window (car elt) (cadr elt))))
         (if (window-live-p save-selected-window-window)
             (select-window save-selected-window-window))))))

(unless (fboundp 'number-sequence)
  ;; snarfed 2005-10-27 from GNU Emacs subr.el (CVS revision 1.484)
  (defun number-sequence (from &optional to inc)
    "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
    (if (or (not to) (= from to))
        (list from)
      (or inc (setq inc 1))
      (when (zerop inc) (error "The increment can not be zero"))
      (let (seq (n 0) (next from))
        (if (> inc 0)
            (while (<= next to)
              (setq seq (cons next seq)
                    n (1+ n)
                    next (+ from (* n inc))))
          (while (>= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc)))))
        (nreverse seq)))))

(unless (fboundp 'help-function-arglist)
  ;; snarfed 2005-12-07 from GNU Emacs help-fns.el (CVS revision 1.80)
  (defun help-function-arglist (def)
    ;; Handle symbols aliased to other symbols.
    (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
    ;; If definition is a macro, find the function inside it.
    (if (eq (car-safe def) 'macro) (setq def (cdr def)))
    (cond
     ((byte-code-function-p def) (aref def 0))
     ((eq (car-safe def) 'lambda) (nth 1 def))
     ((and (eq (car-safe def) 'autoload) (not (eq (nth 4 def) 'keymap)))
      "[Arg list not available until function definition is loaded.]")
     (t t))))

;;; system.el ends here
;;; state.el

;; Copyright (C) 2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar edb--global-state (make-hash-table :test 'eq) "EDB internals.")

(defsubst edb--arrrr! (loc)
  (let ((par edb--global-state))
    (while loc
      (setq par (gethash (car loc) par)
            loc (cdr loc)))
    par))

(defun edb--rinit (loc key &rest init)
  (assert loc)
  (let ((par (edb--arrrr! loc)))
    ;; Return the new hash table.
    (puthash key (apply 'make-hash-table init) par)))

(defun edb--rget (loc key)
  (let ((par (edb--arrrr! loc)))
    (gethash key par)))

(defun edb--rput (loc key value)
  (let ((par (edb--arrrr! loc)))
    (puthash key value par)))

(defun edb--rforget (loc key)
  (let* ((par (edb--arrrr! loc))
         (v (gethash key par)))
    (when (hash-table-p v)
      (clrhash v))
    (remhash key par)))

(edb--rput nil :edb1
	   (make-hash-table :size 7 :test 'eq))
(unless
    (edb--rget
     '(:edb1)
     :seq-funcs)
  (edb--rput
   '(:edb1)
   :seq-funcs
   (make-hash-table :size 3 :test 'eq)))
(unless
    (edb--rget
     '(:edb1)
     :schema-schema)
  (edb--rput
   '(:edb1)
   :schema-schema
   (make-hash-table :size 3 :test 'eq)))
(unless
    (edb--rget
     '(:edb1)
     :bprops)
  (edb--rput
   '(:edb1)
   :bprops
   (make-hash-table :size 11 :test 'eq :weakness 'key)))
(unless
    (edb--rget
     '(:edb1)
     :1singles)
  (edb--rput
   '(:edb1)
   :1singles
   (make-hash-table :test 'eq :weakness 'key)))
(unless
    (edb--rget
     '(:edb1)
     :1moving-mark)
  (edb--rput
   '(:edb1)
   :1moving-mark
   (make-marker)))
(unless
    (edb--rget
     '(:edb1)
     :1displaytypes)
  (edb--rput
   '(:edb1)
   :1displaytypes
   (make-hash-table :test 'eq)))
(unless
    (edb--rget
     '(:edb1)
     :1recordfieldtypes)
  (edb--rput
   '(:edb1)
   :1recordfieldtypes
   (make-hash-table :test 'eq)))

;;;---------------------------------------------------------------------------
;;; Access utilities

(defun edb--mputhash (ht &rest init)
  "Initialize hashtable HT with key1 value1 key2 value2... of INIT."
  (while init
    (puthash (pop init)                 ; DWR: relies on L->R OOE
             (pop init)
             ht)))

(defun edb--hashcollect (what table)
  (let (acc)
    (maphash (case what
               (:keys (lambda (k v) (setq acc (cons k acc))))
               (:vals (lambda (k v) (setq acc (cons v acc))))
               (:cons (lambda (k v) (setq acc (acons k v acc)))))
             table)
    acc))

(defmacro edb--define-child-hash (get key par init)
  `(progn
     (defun ,(intern (format "edb--%s" get)) (,key property)
       (gethash property (gethash ,key ,par)))
     (defun ,(intern (format "edb--%s!" get)) (,key property value)
       (puthash property value (gethash ,key ,par)))
     (defun ,(intern (format "edb--meta%s" get)) (,key &optional command)
       (let* ((par ,par) (me (or (gethash ,key par) (puthash ,key ,init par))))
         (case command
           (:forget (clrhash me) (remhash ,key par))
           (t me))))))

;;;---------------------------------------------------------------------------
;;; Tags

(defun edb-tag (name db)
  "Return the tag object named NAME associated with database DB."
  (edb--1D db name))

(defun edb-tagp (tag record)
  "Return t if RECORD has its TAG set."
  (gethash record tag))

(defun edb-tagx (tag record)
  "Set TAG for RECORD."
  (puthash record t tag))

(defun edb-tag- (tag record)
  "Clear TAG for RECORD."
  (remhash record tag))

;;; state.el ends here
;;; connection.el

;; Copyright (C) 2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(edb--rput '(:edb1 :seq-funcs) 'read-line
  (lambda (finish)
    (let* ((recs (list nil))
           (tp recs))
      (while (< (progn
                  (nconc tp (list (read (current-buffer))))
                  (setq tp (cdr tp))
                  (skip-syntax-forward "^()")
                  (point))
                finish))
      (cdr recs))))

(edb--rput '(:edb1 :seq-funcs) 'write-line
  (lambda (b e recs)
    (goto-char b)
    (delete-region b e)
    (let ((out (current-buffer))
          (print-escape-newlines t))
      (dolist (r recs)
        (prin1 r out)
        (princ "\n" out)))))

;; Each entry is an alist that controls how a schema is handled.
;; Valid keys are:
;;
;; :valid-keys -- list of acceptable keys
;; :valid-options -- list of acceptable options
;; :elaborate-value -- function that takes two args: KEY and FORM,
;;    and returns FORM elaborated as a value (usually by `eval'ing
;;    FORM in a KEY-particular way)
;; :predicates -- list of symbol-predicate pairs; each predicate takes one
;;    arg: OBJECT, and returns non-nil if OBJECT fulfills the predicate
;;
(edb--rput '(:edb1 :schema-schema) 'single
  `((:valid-keys
     (:name stringp)
     (:fields vectorp)
     (:field-separator string-or-rxvec-p)
     (:record-separator string-or-rxvec-p)
     (:record-terminator stringp)
     (:record-separator-function functionp)
     (:read-record functionp)
     (:write-record functionp)
     (:record-defaults functionp)
     (:choose-display functionp)
     :display
     :report
     (:summary-format stringp)
     (:substitutions vectorp)
     (:every-change-function functionp)
     (:first-change-function functionp)
     (:field-order vectorp)
     (:tagged-setup consp)
     (:before-display functionp)
     (:locals vectorp)
     (:substitution-separators vector-of-two-strings-p) ; [fsep rsep]
     (:cruft vectorp)                   ; [[befrec aftrec] [beffld aftfld]]
     :data)
    (:valid-options)
    (:runtime-keys :wraparound :stay-in-edit-mode-p)
    (:elaborate-value
     . ,(lambda (key-ignored x)
          (cond ((and (consp x) (keywordp (car x))) x)
                ((and (consp x) (stringp (car x))) x)
                (t (eval x)))))
    (:predicates
     (string-or-rxvec-p
      . ,(lambda (object)
           (or (stringp object)
               (and (vectorp object)
                    (let ((len (length object)))
                      (and (memq len '(2 3))
                           (stringp (aref object 0))
                           (integerp (aref object 1))
                           (or (= 2 len)
                               (stringp (aref object 2)))))))))
     (vector-of-two-strings-p
      . ,(lambda (object)
           (and (vectorp object)
                (= 2 (length object))
                (let ((v (aref object 0)))
                  (or (not v) (stringp v)))
                (let ((v (aref object 1)))
                  (or (not v) (stringp v)))))))))

(defun edb--validate-schema (type options schema)
  (let* ((ent (or (edb--rget '(:edb1 :schema-schema) type)
                  (error "Invalid schema type: %S" type)))
         (key-specs (cdr (assq :valid-keys ent)))
         (valid-keys (mapcar (lambda (k-ent)
                               (if (consp k-ent)
                                   (car k-ent)
                                 k-ent))
                             key-specs))
         (elaborate-value (cdr (assq :elaborate-value ent)))
         (predicates (let ((local (cdr (assq :predicates ent)))
                           (ht (make-hash-table :test 'eq))
                           name)
                       (dolist (k-ent key-specs)
                         (when (consp k-ent)
                           (setq name (nth 1 k-ent))
                           (unless (gethash name ht)
                             (puthash name (or (cdr (assq name local))
                                               name)
                                      ht))))
                       ht))
         (valid-options (cdr (assq :valid-options ent))))
    ;; check plist form
    (let ((ls schema))
      (while ls
        (unless (keywordp (car ls))
          (error "Not a keyword: %S" (car ls)))
        (setq ls (cddr ls))))
    ;; check key membership
    (let ((ls schema))
      (while ls
        (unless (memq (car ls) valid-keys)
          (error "Not a valid key: %S" (car ls)))
        (setq ls (cddr ls))))
    ;; elaborate values
    (when elaborate-value
      (let ((ls schema) v new)
        (while ls
          (setq v (cadr ls)
                new (funcall elaborate-value (car ls) v))
          (unless (eq new v)
            (setcar (cdr ls) new))
          (setq ls (cddr ls)))))
    ;; check values
    (let ((ls schema) k v k-ent pred-name)
      (while ls
        (when (setq k (car ls)
                    v (cadr ls)
                    k-ent (assq k key-specs)
                    pred-name (and (consp k-ent) (nth 1 k-ent)))
          (unless (funcall (gethash pred-name predicates) v)
            (error "Wrong type for %s (not %s): %S" k pred-name v)))
        (setq ls (cddr ls))))
    ;; check option membership
    (let ((ls options))
      (while ls
        (unless (memq (car ls) valid-options)
          (error "Not a valid option: %S" (car ls)))
        (setq ls (cdr ls)))))
  schema)

(put 'edb--with-callable-connection 'lisp-indent-function 1)
(defmacro edb--with-callable-connection (name &rest body)
  `(flet ((,name (&rest args) (apply ,name args)))
     ,@body))

(defun edb--connect (control-spec-source)
  ;; CONTROL-SPEC-SOURCE is either a filename,
  ;; or a buffer w/ appropriately formatted contents.
  (let ((conn (lexical-let (V)
                (lambda (command &rest args)
                  (case command
                    ;; it's not worthy of emacs if it's not extensible
                    (:V! (setq V (apply 'plist-put V args)))
                    (t (plist-get V command))))))
        schema)
    (edb--with-callable-connection conn
      (with-temp-buffer
        (let (emacs-lisp-mode-hook)
          (emacs-lisp-mode))
        ;; determine schema metainfo
        (let ((reality (cond ((bufferp control-spec-source)
                              (insert-buffer-substring control-spec-source)
                              (list (format "(internal from %S)"
                                            control-spec-source)
                                    (buffer-size)))
                             (t (insert-file-contents control-spec-source))))
              meta)
          (goto-char (point-min))
          ;; skip comments and blank lines before identifying cookie
          (while (and (< (point) (point-max)) (looking-at "[;\n]"))
            (forward-line 1))
          (unless (and (< 4 (cadr reality))
                       (string= ":EDB " (buffer-substring-no-properties
                                         (point) (+ 5 (point))))
                       (consp (setq meta (progn (goto-char (+ 5 (point)))
                                                (read (current-buffer))))))
            (error "Does not seem to be an EDB control file"))
          ;; maintain lameness for the present
          (unless (and (consp meta) (eq 'single (car meta)))
            (error "Not lame enough: %S" meta))
          (conn :V! :schema-type (car meta))
          (conn :V! :schema-options (cddr meta))
          (when (setq schema (cadr meta))
            (flet ((bad (why) (error "Invalid (%s) schema basis: %S"
                                     why schema)))
              (unless (symbolp schema)
                (bad "not a symbol"))
              (unless (boundp schema)
                (bad "unbound variable"))
              (setq schema (symbol-value schema))
              (unless (or (not schema)
                          (consp schema))
                (bad "not a list"))
              (setq schema (reverse schema)))))
        ;; determine schema
        (let (sexp to-eval start kw val)
          (while (< (progn
                      (while (forward-comment 1))
                      (point))
                    (point-max))
            (setq start (point)
                  sexp (read (current-buffer)))
            ;; todo: convert into cond
            (when (consp sexp)
              (push sexp to-eval))
            (when (keywordp sexp)
              (push (setq kw sexp) schema)
              (setq val (read (current-buffer)))
              (if (memq kw '(:display :report :data))
                  (let* ((pls (if (eq t val)
                                  (list :name t :coding t :EOTB ":EOTB")
                                val))
                         (datap (eq :data kw))
                         (tb-start (progn (forward-line 1) (point)))
                         (name (plist-get pls :name))
                         (coding (or (plist-get pls :coding) t))
                         (EOTB (or (plist-get pls :EOTB) ":EOTB"))
                         tb-finish)
                    (unless (or
                             ;; data text blocks are anonymous
                             datap
                             (eq t name) (stringp name))
                      (error "Bad %S name: %S" kw name))
                    (unless (symbolp coding)
                      (error "Bad %S coding: %S" kw coding))
                    (unless (stringp EOTB)
                      (error "Bad %S EOTB: %S" kw EOTB))
                    (setq tb-finish (if (not (re-search-forward
                                              (concat "^" EOTB "$")
                                              (point-max) 1))
                                        (point-max)
                                      (forward-line 1)
                                      (match-beginning 0)))
                    (flet ((pl! (prop val) (setq pls (plist-put pls prop val))))
                      (if datap
                          (let* ((seqr (plist-get pls :seqr))
                                 (f (or (edb--rget '(:edb1 :seq-funcs) seqr)
                                        (error "Bad :seqr func: %S" seqr))))
                            (save-excursion
                              (goto-char tb-start)
                              (pl! :tb-start tb-start)
                              (pl! :tb-finish tb-finish)
                              (pl! :records (funcall f tb-finish))))
                        (pl! :text (buffer-substring-no-properties
                                    tb-start tb-finish))))
                    (if datap
                        (conn :V! (pop schema) pls)
                      (push pls schema)))
                (forward-comment 1)
                (push val schema))))
          ;; validate and stash
          (dolist (form (nreverse to-eval))
            (eval form))
          (conn :V! :schema (edb--validate-schema (conn :schema-type)
                                                  (conn :schema-options)
                                                  (nreverse schema))))))
    conn))

;;;---------------------------------------------------------------------------
;;; connection file cache

(defun edb--connection-file-cache (operation &rest args)
  (let* ((conn (with-current-buffer (edb--S :ddb-spec)
                 (get-text-property (point-min) :connection)))
         (schema (edb--with-callable-connection conn (conn :schema)))
         ;; prevent recursion
         (inhibit-file-name-handlers (cons 'edb--connection-file-cache
                                           inhibit-file-name-handlers))
         name desc)
    (flet ((!! (s) (setq name (substring s (cdr (read-from-string s)))
                         desc (let ((ls schema) look)
                                (while (and (setq ls (memq :display ls)
                                                  look (cadr ls))
                                            (not (string= (plist-get look :name)
                                                          name)))
                                  (setq ls (cddr ls)))
                                look))))
      (case operation
        ((file-exists-p file-readable-p)
         (!! (car args))
         (not (not desc)))
        ((insert-file-contents)
         (!! (car args))
         (insert (plist-get desc :text)))
        ((file-name-directory file-name-nondirectory expand-file-name)
         (car args))
        (t
         (error "badness!"))))))

;;;---------------------------------------------------------------------------
;;; 1.x migration

(defun edb--1mm<-connection (conn)
  (let ((schema (funcall conn :schema))
        v)
    (flet ((S (prop) (plist-get schema prop)))
      (edb--make-v1-monolithic-mess

       :print-name
       (S :name)

       ;; :fieldnames and :recordfieldspecs are set by other means.

       :field-priorities
       (when (setq v (S :field-order))
         (list (mapcar (lambda (spec)
                         (if (consp spec)
                             spec
                           (list spec)))
                       v)))

       :record-sepinfo
       (let* ((cruft (and (S :cruft) (aref (S :cruft) 0)))
              (bef (and cruft (aref cruft 0)))
              (brx (and (vectorp bef) (aref bef 0)))
              (sep (or (S :record-terminator)
                       (S :record-separator)))
              (srx (and (vectorp sep) (aref sep 0)))
              (aft (let ((c (and cruft (aref cruft 1))))
                     (cond ((not c) (S :record-terminator))
                           ((not (setq v (S :record-terminator))) c)
                           ((vectorp c) (vector (concat (regexp-quote v)
                                                        (aref c 0))
                                                (aref c 1)))
                           (t (concat v c)))))
              (arx (and (vectorp aft) (aref aft 0))))
         (edb--make-v1-sepinfo
          :pre-first-string (and (not brx) bef)
          :pre-first-regexp brx
          :pre-first-regexp-submatch (and brx (aref bef 1))
          :sep-string (if srx
                          (when (= 3 (length sep))
                            (aref sep 2))
                        sep)
          :sep-regexp srx
          :sep-regexp-submatch (and srx (aref sep 1))
          :sep-function (S :record-separator-function)
          :post-last-string (and (not arx) aft)
          :post-last-regexp arx
          :post-last-regexp-submatch (and arx (aref aft 1))))

       :field-sepinfo
       (let* ((cruft (and (S :cruft) (aref (S :cruft) 1)))
              (bef (and cruft (aref cruft 0)))
              (brx (and (vectorp bef) (aref bef 0)))
              (sep (S :field-separator))
              (srx (and (vectorp sep) (aref sep 0)))
              (aft (and cruft (aref cruft 1)))
              (arx (and (vectorp aft) (aref aft 0))))
         (edb--make-v1-sepinfo
          :pre-first-string (and (not brx) bef)
          :pre-first-regexp brx
          :pre-first-regexp-submatch (and brx (aref bef 1))
          :sep-string (if srx
                          (when (= 3 (length sep))
                            (aref sep 2))
                        sep)
          :sep-regexp srx
          :sep-regexp-submatch (and srx (aref sep 1))
          ;; sep-function never used, what about the rest?
          :sep-function nil
          :post-last-string (and (not arx) aft)
          :post-last-regexp arx
          :post-last-regexp-submatch (and arx (aref aft 1))))

       :read-record-from-region
       (S :read-record)

       :write-region-from-record
       (let* ((fun (S :write-record))
              ;; poor man's (bootstrapping) nm2no
              (rev (let ((n -1))
                     (mapcar (lambda (spec)
                               (cons (if (symbolp spec)
                                         spec
                                       (car spec))
                                     `(aref record ,(incf n))))
                             (S :fields))))
              (arglist (and fun (help-function-arglist fun)))
              use-let-p)
         (unless arglist
           (setq arglist (mapcar 'car rev)
                 use-let-p t))
         (cond ((not fun)
                nil)
               (use-let-p
                `(lambda (record)
                   (let ,(mapcar (lambda (fname)
                                   `(,fname ,(cdr (assq fname rev))))
                                 arglist)
                     (,fun))))
               (t
                `(lambda (record)
                   (,fun ,@(mapcar (lambda (fname)
                                     (cdr (assq fname rev)))
                                   arglist))))))

       :sub-fieldsep-string
       (when (setq v (S :substitution-separators))
         (aref v 0))

       :sub-recordsep-string
       (when (setq v (S :substitution-separators))
         (aref v 1))

       :substitutions
       (when (setq v (S :substitutions))
         (append v nil))

       :locals
       (mapcar (lambda (x)
                 (if (consp x)
                     (cons (car x) (cadr x))
                   (cons x nil)))
               (S :locals))))))

(defun edb--1format-buffer<-connection (conn)
  (let* ((schema (edb--with-callable-connection conn
                   (conn :schema)))
         (mm (edb--1mm<-connection conn))
         v form)
    (flet ((S (prop) (plist-get schema prop))
           (maybe-quote (v) (if (or (and (consp v)
                                         (memq (car v) '(function lambda)))
                                    (stringp v)
                                    (vectorp v))
                                v
                              (list 'quote v)))
           (ppcur (x) (pp x (current-buffer)) (when (symbolp x) (insert "\n")))
           (maybe-pp () (when form
                          (cond ((vectorp form)
                                 (insert (format "%s:\n" (aref form 0)))
                                 (ppcur (aref form 1)))
                                (t
                                 ;; The local variables block must be 3000
                                 ;; chars from the end, so printing things out
                                 ;; to be handled "normally" loses on large
                                 ;; eval forms.  Kludge around by listing all
                                 ;; eval forms in a text property.  Sigh.
                                 (let ((all (get-text-property 1 :eval-forms)))
                                   (insert
                                    "eval:\n"
                                    "(eval (nth "
                                    (format "%d" (length all))
                                    " (get-text-property 1 :eval-forms)))\n")
                                   (put-text-property
                                    (point-min) (point-max)
                                    :eval-forms (nconc all (list form)))))))))
      (set-buffer (get-buffer-create (format " FORMAT FILE: %S" (S :name))))
      (fundamental-mode)
      (erase-buffer)
      (insert (plist-get (S :display) :text))
      (insert "\n")
      (insert "Local" " " "Variables:\n")
      ;; fields
      (when (setq form nil v (S :fields))
        (setq form `(database-set-fieldnames-to-list database ',(append v nil))))
      (maybe-pp)
      (when (setq form nil v (S :tagged-setup))
        (setq form `(db-tagged-setup ',(cadr v) ,@(cddr v))))
      (maybe-pp)
      ;; monolithic mess
      (setq form nil)
      (dolist (slot (mapcar (lambda (x)
                              (intern (format "database-%s" (car x))))
                            (reverse (get 'edb--v1-monolithic-mess
                                          'cl-struct-slots))))
        (unless (memq slot '(database-fieldnames
                             database-recordfieldspecs
                             database-locals
                             database-record-sepinfo
                             database-field-sepinfo))
          (when (setq v (funcall slot mm))
            (push (maybe-quote v) form)
            (push `(,slot database) form))))
      (when form
        (push 'setf form))
      (maybe-pp)
      ;; locals
      (dolist (local (database-locals mm))
        (setq form `(database-make-local ',(car local) database ,(cdr local)))
        (maybe-pp))
      ;; fsep and rsep
      (let ((sep-slots (mapcar (lambda (x)
                                 (intern (format "sepinfo-%s" (car x))))
                               (reverse (get 'edb--v1-sepinfo
                                             'cl-struct-slots)))))
        ;; fsep
        (setq form nil)
        (dolist (slot sep-slots)
          (when (setq v (funcall slot (database-field-sepinfo mm)))
            (push (maybe-quote v) form)
            (push `(,slot sep) form)))
        (when form
          (setq form `(let ((sep (database-field-sepinfo database)))
                        (setf ,@form))))
        (maybe-pp)
        ;; rsep
        (setq form nil)
        (dolist (slot sep-slots)
          (when (setq v (funcall slot (database-record-sepinfo mm)))
            (push (maybe-quote v) form)
            (push `(,slot sep) form)))
        (when form
          (setq form `(let ((sep (database-record-sepinfo database)))
                        (setf ,@form))))
        (maybe-pp))
      ;; record defaults
      (when (setq form nil v (S :record-defaults))
        (setq form (vector
                    'db-new-record-function
                    `(lambda (rec db)
                       (let ((nm2no (edb--1D db :nm2no))
                             (plist (,v)))
                         (while plist
                           (aset rec (gethash (car plist) nm2no) (cadr plist))
                           (setq plist (cddr plist))))))))
      (maybe-pp)
      ;; before-display stuff (note: clobbered by :choose-display below)
      (when (setq form nil v (S :before-display))
        (setq form (vector
                    'dbf-before-display-record-function
                    `(lambda (record)
                       (,v ,@(mapcar
                              (lambda (field)
                                `(db-record-field record (quote ,field)))
                              (help-function-arglist v)))))))
      (maybe-pp)
      ;; first and every change
      (when (setq form nil v (S :first-change-function))
        (setq form (vector 'dbf-first-change-function v)))
      (maybe-pp)
      (when (setq form nil v (S :every-change-function))
        (setq form (vector 'dbf-every-change-function v)))
      (maybe-pp)
      ;; summary
      (when (setq form nil v (S :summary-format))
        (setq form `(dbf-set-summary-format ,v)))
      (maybe-pp)
      ;; multiple display formats
      (when (setq form nil v (let ((sch (edb--with-callable-connection conn
                                          (conn :schema)))
                                   acc)
                               (while (setq sch (cdr (memq :display sch)))
                                 (push (pop sch) acc))
                               acc))
        (setq form (vector
                    'dbf-format-name-spec-alist
                    (mapcar (lambda (pl)
                              (let ((name (plist-get pl :name)))
                                (cons name (or (plist-get pl :file)
                                               (format "(connection)%s"
                                                       name)))))
                            v))))
      (maybe-pp)
      ;; selecting display format
      (when (setq form nil v (plist-get (edb--with-callable-connection conn
                                          (conn :schema))
                                        :choose-display))
        (setq form (vector
                    'dbf-before-display-record-function
                    `(lambda (record)
                       (db-change-format
                        (,v ,@(mapcar
                               (lambda (field)
                                 `(db-record-field record (quote ,field)))
                               (help-function-arglist v))))))))
      (maybe-pp)
      ;; that's it
      (insert "End:\n")
      ;; remember your roots!
      (put-text-property (point-min) (point-max) :connection conn)
      ;; rv
      (current-buffer))))

(defun edb--1run-hooks (symbol)
  (let ((local (cdr (assq symbol (database-locals dbc-database)))))
    (run-hooks 'local symbol)))

(defun edb--1run-hook-with-arg (symbol &rest args)
  (let ((local (cdr (assq symbol (database-locals dbc-database)))))
    (apply 'run-hook-with-args 'local args))
  (apply 'run-hook-with-args symbol args))

;;; connection.el ends here
;;; db-util.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Lisp utilities.
;; This file is largely cannibalized from util-mde.el and util-mdecl.el,
;; which are available from theory.lcs.mit.edu:/pub/emacs/.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database messages
;;;

(defun db-message (fmtstr &rest args)
  ;; Format message, display it, and log it in buffer *Database-Log*.
  (let ((formatted (apply 'format fmtstr args)))
    (with-current-buffer (get-buffer-create "*Database-Log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert formatted "\n"))
    (message "%s" formatted)))

(defmacro db-warning (fmtstr &rest args)
  `(db-message ,(concat "Warning: " fmtstr) ,@args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching, matching, and replacing
;;;

(defun db-unused-char-in-buffer ()
  "Return a character not used in the current buffer, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (let ((maybe 32)
          (rv t))
      (while (eq rv t)
        (goto-char (point-min))
        (if (not (search-forward (char-to-string maybe) nil t))
            (setq rv maybe)
          (setq maybe (% (1+ maybe) 256))
          (when (eq maybe 32)
            (setq rv nil))))
      rv)))

(defun db-unused-char-in-string (s)
  "Return a character not used in string S, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (with-temp-buffer
    (insert s)
    (db-unused-char-in-buffer)))

;;; Skipping

(defsubst db-skip-string-forward (s)
  (or (string= "" s)
      (let* ((beg (point))
             (len (length s))
             (end (+ beg len)))
        (when (and (<= end (point-max))
                   (string= s (buffer-substring-no-properties beg end)))
          (forward-char len)
          t))))

(defsubst db-skip-string-backward (s)
  (or (string= "" s)
      (let* ((end (point))
             (len (length s))
             (beg (- end len)))
        (when (and (<= (point-min) beg)
                   (string= s (buffer-substring-no-properties beg end)))
          (forward-char (- len))
          t))))

(defsubst db-skip-regexp-forward (rx)
  "If point is at regexp RX, move past it and return point;
otherwise return nil."
  (when (looking-at rx)
    (goto-char (match-end 0))))

;; From Robert Potter <potter@silver.lcs.mit.edu>
(defun db-looking-back-at (rx)
  "Return t when text before point matches regular expression RX."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (re-search-backward (concat "\\(" rx "\\)\\'") (point-min) t))))

(defun db-how-many-string-overlapping (s)
  "Return number of matches for string S following point,
including overlapping ones."
  (let ((count 0))
    (save-excursion
      (while (search-forward s nil t)
        (goto-char (1+ (match-beginning 0)))
        (incf count)))
    count))

(defun db-how-many-substring-overlapping (small big)
  "Return number of matches for SMALL in BIG (both strings),
including overlapping ones."
  (let ((rx (regexp-quote small))
        (count 0)
        (start -1))
    (while (setq start (string-match rx big (1+ start)))
      (incf count))
    count))

(defun db-find-char (c s &optional count)
  "Look for char C in string S; return first index in S whose element is C.
If optional arg COUNT is specified, return the COUNTth occurrance."
  (unless count
    (setq count 1))
  (let ((idx 0)
        (len (length s))
        rv)
    (while (and (< idx len) (not rv))
      (when (char-equal c (aref s idx))
        (if (= count 1)
            (setq rv idx)
          (decf count)))
      (incf idx))
    rv))

(defun db-find-char-from-end (c s &optional count)
  "Look for char C in string S; return last index in S whose element is C.
Optional arg COUNT means return the COUNTth occurrance from the end."
  (unless count
    (setq count 1))
  (let ((idx (1- (length s)))
        rv)
    (while (and (> idx -1) (not rv))
      (when (char-equal c (aref s idx))
        (if (= count 1)
            (setq rv idx)
          (decf count)))
      (decf idx))
    rv))

(defsubst db-string-trim-whitespace (s)
  "Return a substring of S with whitespace removed from beginning and end."
  (let* ((beg (when (string-match "\\`[ \t\n\f\r]+" s)
                (match-end 0)))
         (end (string-match "[ \t\n\f\r]+\\'" s (or beg 0))))
    (if (or beg end)
        (substring s (or beg 0) end)
      s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defun edb--G  (key)       (edb--rget '(:edb1) key))
(defun edb--G! (key value) (edb--rput '(:edb1) key value))

;; Cache for buffer-local control properties hash table.
(defvar edb--bprops nil)

(defmacro edb--S (property)
  `(gethash ,property edb--bprops))

(defmacro edb--S! (property newvalue)
  `(puthash ,property ,newvalue edb--bprops))

(defun edb-get (property &optional buffer)
  (unless (let* ((single (edb--rget '(:edb1 :schema-schema) 'single))
                 (runtime (cdr (assq :runtime-keys single))))
            (memq property runtime))
    (error "Bad property: %S" property))
  (if buffer
      (edb--rget (list :edb1 :bprops buffer) property)
    (edb--S property)))

(defun edb-put (property newvalue &optional buffer)
  (unless (let* ((single (edb--rget '(:edb1 :schema-schema) 'single))
                 (runtime (cdr (assq :runtime-keys single))))
            (memq property runtime))
    (error "Bad property: %S" property))
  (if buffer
      (edb--rput (list :edb1 :bprops buffer) property newvalue)
    (edb--S! property newvalue)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;

(defun db-locate-readable-file-prefer-cwd (filename path &optional suffixes)
  "Return the full path of a readable file named FILENAME, located in
`default-directory' or on PATH, a list of directories (strings).
Optional arg SUFFIXES is a list of suffixes to append to FILENAME
when searching."
  (setq path (cons default-directory path))
  ;; The following is more or less equivalent to:
  ;;   (locate-file filename path suffixes 'readable)
  ;; but `locate-file' is not yet (2005-01-18) widely available.
  (catch 'full
    (dolist (dir path)
      (unless dir
        (setq dir default-directory))
      (dolist (suf (or suffixes '("")))
        (let ((try (expand-file-name (concat filename suf) dir)))
          (when (file-readable-p try)
            (throw 'full try)))))
    nil))

(defun db-same-file-p (n1 n2)
  "Return t if N1 and N2 are names for the same file.
Return nil if neither N1 nor N2 names an existing file."
  (setq n1 (file-chase-links n1)
        n2 (file-chase-links n2))
  (or (equal n1 n2)
      (equal n1 (file-name-nondirectory n2))
      (equal n2 (file-name-nondirectory n1))
      (and (file-exists-p n1) (file-exists-p n2)
           (equal (file-attributes n1)
                  (file-attributes n2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion
;;;

;; string-to-int is unacceptable because it returns 0 for unparseable values.

(defun db-string->integer-or-nil (s)
  (let ((rv (condition-case nil
                (car (read-from-string s))
              (error nil))))
    (and (integerp rv) rv)))
(defalias 'db-string->number-or-nil 'db-string->integer-or-nil)

(defun db-string->integer (s)
  "Return the integer represented by string S, or err."
  (or (db-string->integer-or-nil s)
      (error "db-string->integer: `%s' doesn't look like an integer." s)))
(defalias 'db-string->number 'db-string->integer)

(defun db-number-or-nil->string (n)
  (if (numberp n)
      (number-to-string n)
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;

(defmacro db-in-buffer (buffer &rest body)
  ;; This macro, which works when body moves point in a buffer
  ;; displayed in a window other than the selected window, is from
  ;; Joe Wells <jbw@cs.bu.edu>.  (If Lisp code moves point in a
  ;; buffer displayed in a window other than the selected window,
  ;; Emacs kindly restores point in the buffer to its window's
  ;; version of point.)
  (declare (indent 1) (debug (form body)))
  `(let ((targ ,buffer)
         (this-buffer (current-buffer))
         (db-in-buffer-thunk (lambda () ,@body)))
     (if (eq targ this-buffer)
         (funcall db-in-buffer-thunk)
       ;; Can't use save-excursion here because we only want to save the
       ;; current buffer, not its value for point.
       (unwind-protect
           (progn
             (set-buffer targ)
             (let* ((w (get-buffer-window targ))
                    (trackp (and (not (eq w (selected-window)))
                                 (eq (window-point w) (point)))))
               (prog1
                   (funcall db-in-buffer-thunk)
                 (when (and trackp
                            (eq (current-buffer) targ)
                            (eq w (get-buffer-window targ))
                            (not (eq w (selected-window))))
                   (set-window-point w (point))))))
         (if (and (bufferp this-buffer) (buffer-name this-buffer))
             (set-buffer this-buffer))))))


(defun db-copy-buffer-local-variables (buf &rest exclude)
  "Copy the values of most of BUF's local variables into the current buffer.
The var `enable-multibyte-characters' is handled by `set-buffer-multibyte'
and `buffer-undo-list' is excluded along with any of the other variable names
\(symbols\) listed in EXCLUDE."
  (let (symbol value)
    (dolist (pair (with-current-buffer buf (buffer-local-variables)))
      (unless (or (atom pair)
                  (memq (setq value (cdr pair)
                              symbol (car pair))
                        exclude))
        (case symbol
          ((0 nil buffer-undo-list))    ; do nothing
          (enable-multibyte-characters
           (set-buffer-multibyte value))
          (t
           (set (make-local-variable symbol) value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

(defun db-string-split-last-word (s &optional butnot)
  "Return list of two strings (all-but-last-word last-word).
If there is only one word, return (S \"\").
The result strings can be concatenated to return the original string,
with the exception of some number (at least one) of spaces and tabs,
and possibly a comma immediately preceding them.
Optional arg BUTNOT, if non-nil, is a regexp (containing spaces or tabs)
which, if found at the end of S, should be considered a single word."
  (if (let ((delim ",?[ \t]+"))
        (or (and butnot (string-match (concat delim "\\(" butnot "\\)$") s))
            (string-match (concat delim "\\([a-zA-Z0-9'-]+\\)$") s)))
      (list (substring s 0 (match-beginning 0))
            (substring s (match-beginning 1)))
    (list s "")))

(defun db-string-split-first-word (s)
  "Return list of strings (first-word remaining-words).
String S is split at the first sequence of spaces and tabs."
  (if (string-match "[ \t]+" s)
      (list (substring s 0 (match-beginning 0))
            (substring s (match-end 0)))
    (list s "")))

(defun db-count-newlines (s)
  "Return the number of newline characters found in string S."
  (let ((rv 0))
    (dotimes (idx (length s))
      (when (char-equal ?\n (aref s idx))
        (incf rv)))
    rv))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor movement
;;;

(defun db-forward-line-wrapping (arg)
  "Like forward-line, but wrap around to the beginning of the buffer if
it encounters the end."
  (interactive "p")
  (let ((left (forward-line arg)))
    (cond ((or (> left 0) (not (bolp)))
           (goto-char (point-min))
           (db-forward-line-wrapping left))
          ((< left 0)
           (goto-char (point-max))
           (db-forward-line-wrapping (1+ left))))))

(defun db-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File local variables
;;;

(defun edb-true (&rest args)
  "Return t unconditionally, no matter the ARGS."
  t)

(defun edb-hookp (val)
  "Return t if VAL appears to be a hook.
That is, if it is satisfies `functionp', or it is a list of
elements each of which is either t or satisfies `functionp'."
  (or (functionp val)
      (and (consp val)
           (equal (make-list (length val) t)
                  (mapcar (lambda (v)
                            (or (eq t v)
                                (functionp v)))
                          val)))))

(defun db-really-hack-local-variables ()
  "Like `hack-local-variables', but without inhibitions."
  (let ((enable-local-eval t)
        (enable-local-variables t))
    (hack-local-variables)))


;;; db-util.el ends here
;;; db-rep.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Representation and basic operations for
;; database, recordfieldspec objects.

;;; Code:

(edb--define-child-hash 1D db (edb--G :1singles)
                        (let ((new (make-hash-table :size 7)))
                          (flet ((ahash () (make-hash-table
                                            :test 'eq
                                            :weakness 'key)))
                            (edb--mputhash new
                                           :markedp (ahash)
                                           :hiddenp (ahash))
                            new)))

(defun edb--1all-known-databases ()
  (edb--hashcollect :keys (edb--G :1singles)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database abstraction
;;;

(put 'edb :databases-made 0)

(defstruct (edb--v1-monolithic-mess
            (:type vector)
            (:constructor edb--make-v1-monolithic-mess)
            (:conc-name database-)
            (:copier edb--copy-v1-monolithic-mess))
  (print-name (format "Unnamed Database %d" (incf (get 'edb :databases-made))))

  ;; field information
  fieldnames                            ; this is repeated in the recordpsecs
  recordfieldspecs                      ; vector: symbols or recordfieldspecs
                                        ; if symbol, search :1recordfieldtypes

  field-priorities                      ; maybe call this order-fields instead

  ;; For file i/o
  (record-sepinfo (edb--make-v1-sepinfo) :read-only t)
  (field-sepinfo (edb--make-v1-sepinfo) :read-only t)
  read-record-from-region
  write-region-from-record
  sub-fieldsep-string
  sub-recordsep-string

  substitutions                         ; associate ACTUAL with STORED strings

  locals                                ; associate SYMBOL with VALUE
  )

;;; Accessors

(defun database-set-modified-p (db val)
  (unless (eq val (edb--1D db :modp))
    (edb--1D! db :modp val)
    ;; Reflect the change in each data display buffer.
    (mapc (lambda (ddb)
            (with-current-buffer ddb
              (set-buffer-modified-p val)
              (force-mode-line-update)))
          (edb--1D db :ddbufs))))

(defun database-file (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :file))

(defun database-data-display-buffers (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :ddbufs))

(defun database-no-of-records (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :nrecords))

;;; Non-primitive accessors

(defun db-rs-slice (db which)
  (let ((rs-slices (edb--1D db :rs-slices)))
    (or (gethash which rs-slices)
        (puthash which (let* ((all-rs (edb--1D db :elaborated-rfspecs))
                              (len (length all-rs))
                              (v (make-vector len nil)))
                         (do ((i 0 (1+ i)))
                             ((= len i))
                           (aset v i (funcall which (aref all-rs i))))
                         v)
                 rs-slices))))

;;; Database-local variables

(defun database-make-local (symbol db &optional value)
  "Declare a database-local variable named by SYMBOL for DATABASE.
Each such variable should only be declared once.
If optional argument VALUE is specified, the variable is set to it."
  (let ((lookup (assq symbol (database-locals db))))
    (if lookup
        (error "%s is already defined as a local variable in %s."
               symbol (database-print-name db))
      (push (cons symbol value) (database-locals db)))))

(defun database-set-local (symbol db value &optional no-error)
  "Set the value of database-local variable SYMBOL, in DB, to VALUE.
SYMBOL must have been declared by a previous call to `database-make-local'
unless optional argument NO-ERROR is supplied, in which case the function
does that automatically, if necessary."
  (let ((lookup (assq symbol (database-locals db))))
    (if lookup
        (setcdr lookup value)
      (if no-error
          (database-make-local symbol db value)
        (error "%s is not a database-local variable for %s."
               symbol (database-print-name db))))))

(defun database-get-local (symbol db &optional no-error)
  "Return the value of database-local variable SYMBOL for DATABASE.
If SYMBOL was not declared by a previous call to `database-make-local',
an error is signalled unless optional argument NO-ERROR is non-nil,
in which case nil is returned."
  (let ((lookup (assq symbol (database-locals db))))
    (cond (lookup
           (cdr lookup))
          (no-error
           nil)
          (t
           (error "%s is not a database-local variable for %s."
                  symbol (database-print-name db))))))

(defun database-local-p (symbol db)
  "Return non-nil if SYMBOL is a database-local variable for DATABASE."
  (assq symbol (database-locals db)))

;;; Non-primitive setters

(eval-when-compile (defvar db-default-field-type))

(defun database-set-fieldnames-to-list (db fspecs &optional dtype)
  "Set DB's fieldnames and record field types according to FSPECS.
But do nothing if DB's `fieldnames' slot is already set.

FSPECS is a list, each element of which can either be a single symbol,
the field name, or a cons (NAME . RECORDFIELDTPYE).  Optional third arg
DTYPE specifies the default recordfieldtype for single-symbol elements.
If DTYPE is not given, use the value of `db-default-field-type' (NOTE:
this variable is deprecated) if bound, or `string' otherwise."
  (unless (database-fieldnames db)
    (let* ((len (length fspecs))
           (fno 0)
           (rs  (make-vector len nil))
           (vec (make-vector len nil))
           (n2n (make-hash-table :size len :test 'eq))
           ;; We used to use a buffer-local var `db-default-field-type', but
           ;; that was a bug; a field's type is a property of the field, not
           ;; of its display, not of a buffer.  Generally, the concepts of
           ;; "local" and "default" together are difficult to explain w/o
           ;; detailed knowledge of the implementation and its precise timing
           ;; of events.  Better to specify the default together with the
           ;; non-defaults, such as is now possible w/ DTYPE, and let this
           ;; function worry about making the correct associations.
           ;;
           ;; For EDB 1.x, although we are obligated to check for use of this
           ;; var, we are free to drop the buffer-local var, as long as we
           ;; explain ourselves clearly in the documentation.
           (default-type (cond (dtype)
                               ((and (boundp 'db-default-field-type)
                                     db-default-field-type))
                               (t 'string))))
      (dolist (fname fspecs)
        (let ((type (if (consp fname)
                        (prog1 (cdr fname)
                          (setq fname (car fname)))
                      default-type)))
          (if (gethash type (edb--G :1recordfieldtypes))
              (aset rs fno type)
            (error "bad type %s" type))
          (aset vec fno fname)
          (puthash fname fno n2n)
          (incf fno)))
      (edb--1D! db :nm2no n2n)
      ;; elaborate
      (let ((elab (copy-sequence rs))
            one-rs)
        (do ((fno 0 (1+ fno)))
            ((= len fno))
          (when (symbolp (setq one-rs (aref elab fno)))
            (aset elab fno (db-rfspec<-rftype one-rs))))
        (edb--1D! db :elaborated-rfspecs elab))
      (edb--1D! db :rs-slices (make-hash-table :size 5 :test 'eq))
      (setf (database-fieldnames db) vec
            (database-recordfieldspecs db) rs))))

;;; Not quite so basic functions.

(defun database-full-fieldsep-string (db)
  ;; Return the string that really separates database record fields.
  (if (database-write-region-from-record db)
      nil
    (sepinfo-sep-string (database-field-sepinfo db))))

(defun database-full-recordsep-string (db)
  ;; Return the string that really separates database records.
  (let ((rsep (database-record-sepinfo db)))
    (if (database-write-region-from-record db)
        (sepinfo-sep-string rsep)
      (let ((fsep (database-field-sepinfo db)))
        (concat (sepinfo-post-last-string fsep)
                (sepinfo-sep-string rsep)
                (sepinfo-pre-first-string fsep))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recordfieldspec abstraction
;;;

(defstruct (edb--v1-rs
            (:type vector) :named
            (:constructor edb--make-v1-rs)
            (:conc-name edb--1rs-)
            (:copier edb--copy-v1-rs))

  ;; datatype information
  type                                  ; e.g. 'string
  default-value
  common-form-function
  merge-function

  order-fn                              ; (A B) => {-1,0,1}
  sort-fn                               ; (A B) => bool
  match-function                        ; (PATTERN OBJ) => bool

  help-info                             ; perhaps should be help-string;
                                        ; or should be more complicated.

  actual->stored
  stored->actual

  ;; customizations
  constraint-function)

(put 'edb--v1-rs 'kwidx                 ; poor man's defsetf --ttn
     (let ((idx 0))
       (mapcar (lambda (ent)
                 (cons (intern (format ":%s" (symbol-name (car ent))))
                       (incf idx)))
               (cdr (get 'edb--v1-rs 'cl-struct-slots)))))


(defun db-rs-sortfunc (rs &optional reversep)
  "Return a sort function for records described by recordfieldspec RS.
If optional argument REVERSEP is non-nil, then the sort function goes in
the opposite order.
If the sort-fn slot of the appropriate recordfieldspec of `database' doesn't
contain one, one is made up on the fly from the order-fn slot.
If the order-fn slot is also empty, the resulting function always returns
nil, indicating that it is not the case that the first argument is less
than the second."
  (let ((sort-fn (edb--1rs-sort-fn rs)))
    (if sort-fn
        (if reversep
            `(lambda (value1 value2)
               ;; Maintain arg order and invert the result, so as
               ;; to properly handle the case: (eq value1 value2).
               (not (,sort-fn value1 value2)))
          sort-fn)
      (let ((order-fn (edb--1rs-order-fn rs)))
        (if order-fn
            `(lambda ,(if reversep '(value2 value1) '(value1 value2))
               (= -1 (funcall (function ,order-fn) value1 value2)))
          'nil-function)))))

(defun db-rs-ordfunc (rs &optional reversep)
  "Return an order function for records described by recordfieldspec RS.
If optional argument REVERSEP is non-nil, then the order function goes in
the opposite order.
If the order-fn slot of the appropriate recordfieldspec of `database' doesn't
contain one, one is made up on the fly from the sort-fn slot; `equal'
is used to determine whether two records are equal.
If the sort-fn slot is also empty, the resulting function always
returns 0, indicating equality."
  (let ((order-fn (edb--1rs-order-fn rs)))
    (if order-fn
        (if reversep
            `(lambda (value1 value2)
               (,order-fn value2 value1))
          order-fn)
      (let ((sort-fn (edb--1rs-sort-fn rs)))
        (if sort-fn
            `(lambda ,(if reversep '(value2 value1) '(value1 value2))
               (cond ((equal value1 value2)
                      0)
                     ((funcall (function ,sort-fn) value1 value2)
                      -1)
                     (t
                      1)))
          (lambda (value1 value2) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records
;;;

;; Abstraction

(defun db--mkrec (nfields nm2no init)
  (let ((rv (make-vector nfields nil)))
    (cond ((eq :alist (car init))       ; alist
           (dolist (pair (cdr init))
             (aset rv (gethash (car pair) nm2no)
                   (cdr pair))))
          (t                            ; plist
           (while init
             (aset rv (gethash (pop init) nm2no)
                   (pop init)))))
    rv))

(defun db-make-record (database init)
  "Return a DATABASE-specific record initialized with INIT.
INIT is either a list of alternating fieldnames (symbols) and values,
or a list whose car is the keyword `:alist' and whose cdr is an alist
mapping fieldnames to their values."
  (db--mkrec (length (database-fieldnames database))
             (edb--1D database :nm2no)
             init))

(defun db-copy-r2r (source target)
  "Copy the field values of the SOURCE record to the TARGET record."
  (dotimes (fno (length source))
    (aset target fno (aref source fno))))

;;; Fieldnames and record fieldnumbers

(defsubst db-fname<-fno (fieldnumber db)
  "Given a record FIELDNUMBER and DB, return a record fieldname."
  (aref (database-fieldnames db) fieldnumber))

;;; Retrieving field values

(defun db-record-field (record fieldname &optional database)
  "Return from RECORD the field with name FIELDNAME.
If RECORD is t, use the \"current record\".  Optional third
argument DATABASE specifies a database other than the current one."
  (let* ((db (or database dbc-database))
         (fno (or (gethash fieldname (edb--1D db :nm2no))
                  (error "No %s field in current record." fieldname)))
         (rec (if (eq t record)
                  (edb--S :under)
                record)))
    (aref rec fno)))

;;; Checking constraints

(defun db-check-constraint (value rec idx db)
  (let ((func (aref (db-rs-slice db 'edb--1rs-constraint-function) idx)))
    (when (and func (not (funcall func value rec idx db)))
      (error "The value `%s' does not satisfy the constraint for field %s."
             value (db-fname<-fno idx db)))))

;;; Setting field values

(defun db-record-set-field (record fieldname value &optional database nocheck)
  "Set, in RECORD, field FIELDNAME to VALUE.  Fourth argument is DATABASE.
Check constraints first unless optional fifth argument NOCHECK is non-nil."
  (let* ((db (or database dbc-database))
         (fno (or (gethash fieldname (edb--1D db :nm2no))
                  (error "No %s field in current record." fieldname))))
    (unless (or nocheck (not db))
      (db-check-constraint value record fno db))
    (aset record fno value)))


;;; Setting fields in :under

(defsubst dbf-this-record-set-field (fieldname value)
  "Set field with name FIELDNAME in `:under' to VALUE.
Causes the entire record to be redisplayed pretty soon.
You may want to use `dbf-displayed-record-set-field' instead."
  ;; fixme: check `(edb--S :utkmodp)' first. --ttn
  (db-record-set-field (edb--S :under) fieldname value)
  (setq dbf-redisplay-entire-record-p t))

;;; The displayed record

(defsubst dbf-displayed-record-field (fieldname)
  "Return the value of the field named FIELDNAME from the displayed record."
  (db-record-field (dbf-displayed-record) fieldname))

(defun dbf-displayed-record-set-field (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed soon."
  ;; Make sure displayed-record = this-record.
  (dbf-set-this-record-modified-p t)
  (dbf-this-record-set-field fieldname value))

(defsubst dbf-displayed-record-set-field-and-redisplay (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed immediately."
  ;; Is this correct?  Maybe displayed-record != this-record.
  (dbf-this-record-set-field fieldname value)
  (dbf-redisplay-entire-record-maybe))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sepinfo abstraction
;;;

;; This tells how a list of information appears in a file.

(defstruct (edb--v1-sepinfo
            (:type vector)
            (:constructor edb--make-v1-sepinfo)
            (:conc-name sepinfo-))
  pre-first-string
  pre-first-regexp
  pre-first-regexp-submatch
  sep-string
  sep-regexp
  sep-regexp-submatch
  sep-function                          ; return (end-pos . next-start-pos)
                                        ; takes prev-end-pos as an argument
                                        ; next-start-pos nil for last record
  post-last-string
  post-last-regexp
  post-last-regexp-submatch)


(defun db-make-n-line-sep-function (n)
  "Return a function useful when all records have exactly N lines on disk.
This is for use with the `:record-separator-function' control property."
  `(lambda (prev-end)
     (forward-line ,n)
     (cons (point) (unless (eobp) (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating database records
;;;

(defun database-add-record (record db &optional location)
  ;; Add RECORD to DATABASE.  If optional third argument LOCATION is a number,
  ;; insert immediately before that index; if it is a function, call it with
  ;; the current index to get the new index (modulo operation included); if it
  ;; is nil, insert at the end.
  (let* ((vov (edb--1D db :vov))
         (vsz (length vov))
         (new vov)
         (bno (edb--1D db :nrecords))   ; before
         (atz (cond ((numberp location) ; 0-based index to insert at
                     (1- location))
                    ((functionp location)
                     (% (1- (funcall location (edb--S :index))) bno))
                    ((not location)
                     bno))))
    ;; realloc if necessary
    (unless (< bno vsz)
      (setq new (make-vector (+ 10 bno) nil)) ; todo: parameterize
      (dotimes (i atz)
        (aset new i (aref vov i))))
    ;; shift forward to leave a hole
    (do ((i bno (1- i)))
        ((= atz i))
      (aset new i (aref vov (1- i))))
    ;; add it
    (aset new atz record)
    (unless (eq new vov)
      (edb--1D! db :vov new))
    (edb--1D! db :nrecords (1+ bno))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping over a database
;;;

(defvar db-inform-interval 100
  "When doing a lengthy computation, inform the user of progress every this
many records.  If nil, don't inform.")

;; Mapping functions dynamically bind `db-lmap-index'.

(defvar db-lmap-index nil
  "The current index in a call to `db-lmap'.")

(defun db-lmap (lmapfunc db &optional hide message accumulate)
  ;; Use LMAPFUNC instead of simply FUNC to avoid a dynamic binding conflict.
  ;; In the body, variable `db-lmap-index' is the index of the record.
  ;; See `db-maprecords'.
  (let* ((vov (edb--1D db :vov))
         (db-lmap-index 1)
         (htag (edb-tag :hiddenp db))
         (results (when accumulate (list nil)))
         (--hit (assq 'db-inform-interval (database-locals db)))
         (--infint (if --hit (cdr --hit) db-inform-interval))
         (tp results)
         lmaprecord v)
    (setq message (and --infint message))
    (dotimes (i (edb--1D db :nrecords))
      (setq db-lmap-index (1+ i)
            lmaprecord (aref vov i))
      (unless (and hide (edb-tagp htag lmaprecord))
        (setq v (funcall lmapfunc lmaprecord))
        (when accumulate
          (setcdr tp (list v))
          (setq tp (cdr tp))))
      (when (and message (zerop (% db-lmap-index --infint)))
        (db-message message db-lmap-index)))
    (when accumulate
      (cdr results))))

(defun db-maprecords (func &optional db hide message accumulate)
  "Apply FUNC to every record in current database in order of ascending index.
Optional second arg DB specifies a database to use other than the current one.
Optional third arg HIDE non-nil means apply FUNC only to unhidden records.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' records.  If optional fifth arg ACCUMULATE is non-nil,
return a list of the results; otherwise return nil."
  (db-lmap (lambda (record)
             (funcall func record))
           (or db dbc-database) hide message accumulate))

(defalias 'maprecords 'db-maprecords)

;;; db-rep.el ends here
;;; db-format.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Displaying and editing database records.

;;; Code:

(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;
;; Location in the format (current field info)
;;

;; All of these variables include "this-" in their names.

(defsubst dbf-this-field-name (this-ds)
  (and this-ds (db-fname<-fno (edb--1ds-record-index this-ds)
                              dbc-database)))

(defsubst dbf-this-field-text ()
  ;; The text actually in the buffer.
  (buffer-substring-no-properties (edb--S :fbeg) (dbf-this-field-end-pos)))

(defsubst dbf-set-this-field-text (field-text)
  "Make the format display FIELD-TEXT in the current field."
  ;; Set the text actually in the buffer.
  (let ((beg (edb--S :fbeg)))
    (delete-region beg (dbf-this-field-end-pos))
    (goto-char beg))
  (insert field-text))

(defsubst dbf-this-field-modified-p ()
  (buffer-modified-p))
(defsubst dbf-set-this-field-modified-p (arg)
  (set-buffer-modified-p arg))


;;
;; The displayed record
;;

(put (quote dbf-set-this-record-modified-function) (quote safe-local-variable) (quote edb-true))
(defvar dbf-set-this-record-modified-function nil
  "A function called when the current record is marked as modified.
The function takes no arguments and its return value is ignored.
It is called after `:original' is copied to `:under' and after
`:utkmodp' is set to t.")

(defsubst dbf-set-this-record-modified-p (arg)
  "Set the value of `:utkmodp' to ARG.
If ARG is non-nil and `:utkmodp' is nil, also do the necessary
record-copying and call `dbf-set-this-record-modified-function'."
  (let ((cur (edb--S :utkmodp)))
    (edb--S! :utkmodp arg)
    (cond ((and arg (not cur))
           (db-copy-r2r (edb--S :original) (edb--S :under))
           (edb--1run-hooks 'dbf-set-this-record-modified-function)))))

(defsubst dbf-displayed-record ()
  "Return the record currently displayed in this data display buffer.
This is `:under' if `:utkmodp' is non-nil and `:original' otherwise."
  (if (edb--S :utkmodp)
      (edb--S :under)
    (edb--S :original)))

(defvar dbf-redisplay-entire-record-p nil
  "T if the whole record needs to be redisplayed.
This is often set by change functions.")


;;
;; Hooks
;;

;;; Minor mode hooks

(defvar db-view-mode-hooks nil
  "Function or list of functions called when Database View mode is entered.")

(defvar db-edit-mode-hooks nil
  "Function or list of functions called when Database Edit mode is entered.")

;;; Movement hooks

(put (quote dbf-before-display-record-function) (quote safe-local-variable) (quote edb-true))
(defvar dbf-before-display-record-function nil
  "A function called before a record is displayed.
The function takes one argument, the record.

This is a good place to put calls to `db-change-format'.  Depending on
your function's implementation, however, you may silently override any user
calls to that function.")

(put (quote dbf-enter-field-hook) (quote safe-local-variable) (quote edb-true))
(defvar dbf-enter-field-hook nil
  "A function (of no arguments) called whenever a display field is entered.")

;;; Change hooks

(put (quote dbf-first-change-function) (quote safe-local-variable) (quote edb-true))
(defvar dbf-first-change-function nil
  "A function called the first time a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

(put (quote dbf-every-change-function) (quote safe-local-variable) (quote edb-true))
(defvar dbf-every-change-function nil
  "A function called whenever a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

(defun dbf-set-change-function (fieldname function)
  "Set the change function for FIELDNAME to FUNCTION in the current database.
FUNCTION takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed."
  (aset (edb--S :change-functions)
        (gethash fieldname (edb--1D dbc-database :nm2no))
        function))

(put (quote dbf-after-record-change-function) (quote safe-local-variable) (quote edb-true))
(defvar dbf-after-record-change-function nil
  "Function called whenever changes to a record are recorded semi-permanently
by `dbf-process-current-record-maybe'.  For convenience, the function
takes the record as an argument, which is guaranteed to be `:under'.
Its return value is ignored.")


;;
;; The format
;;

;; Some variables local to the data display buffer don't need to be changed
;; when the display format changes.  The ones appearing below do.

(put (quote dbf-format-name-spec-alist) (quote safe-local-variable) (quote edb-true))
(defvar dbf-format-name-spec-alist nil
  "Association list of format names and format specifiers.
Each format name is an arbitrary string.
A format specifier is a filename or format file specifier, which is
a list of values for format variables.
The user sets the format specifier to a filename, and after that format file
has been read, EDB replaces the filename with a list of values for format
variables, so that the file need not be read again.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;

(defconst db-ds+opts-rx (concat
                         "\\\\"
                         "\\([[:alpha:]][[:alnum:]<>,=-]*\\)" ; 1
                         "\\(\\\\ \\)?"))                     ; 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstraction
;;;

;;
;; Displayspec
;;

;; Besides `record-index' all other information has to do with display only.

(defstruct (edb--1ds
            (:type vector) :named
            (:constructor edb--make-1ds)
            (:copier edb--copy-1ds))

  record-index                          ; zero-based backref

  ;; size and shape
  indent
  min-width
  max-width
  min-height                            ; default 1
  max-height                            ; default 1
  min-bytes
  max-bytes

  ;; other display info
  truncation-display-action
  padding-action
  actual->display
  display->actual
  ;; Is this where these belong?  Well, it lets me not make a new displayspec
  ;; for them...
  match-actual->display
  match-display->actual

  ;; editing info
  truncation-editing-action
  (reachablep t))

(put 'edb--1ds 'kwidx                   ; poor man's defsetf --ttn
     (let ((idx 0))
       (mapcar (lambda (ent)
                 (cons (intern (format ":%s" (symbol-name (car ent))))
                       (incf idx)))
               (cdr (get 'edb--1ds 'cl-struct-slots)))))


;;
;; Optspecinfo
;;

;; An optspecinfo tells how to interpret optional parameters to a
;; display field specification.  An optspecinfo is a three-element list of
;;  * param-name: a string
;;  * settor: function taking a displayspec and a value and setting a slot
;;  * opt-param->value: a function converting the optional parameter (that
;;    is, the string that follows the equal sign) into the actual value.

(defconst db-optspec-list
  (mapcar (lambda (x)
            `(,(nth 0 x)
              ,(flet ((dset (frag) (let ((f (intern (format "edb--1ds-%s"
                                                            frag))))
                                     `(lambda (ds val)
                                        (setf (,f ds) val)))))
                 ;; Settor-or-accessor is either a settor function, a
                 ;; slotname, or a list of slotnames.  In the latter two
                 ;; cases, it's first converted into a settor.
                 (let ((settor-or-accessor (nth 1 x)))
                   (cond ((functionp settor-or-accessor)
                          settor-or-accessor)
                         ((symbolp settor-or-accessor)
                          (dset settor-or-accessor))
                         (t
                          `(lambda (displayspec value)
                             ,@(mapcar (lambda (frag)
                                         `(,(dset frag) displayspec value))
                                       settor-or-accessor))))))
              ,(nth 2 x)))
          '(("indent" indent (lambda (x) t))
            ("noindent" indent (lambda (x) nil))

            ("width" (min-width max-width) db-string->number)
            ("min-width" min-width db-string->number)
            ("max-width" max-width db-string->number)
            ("length" (min-width max-width) db-string->number)
            ("min-length" min-width db-string->number)
            ("max-length" max-width db-string->number)
            ("height" (min-height max-height) db-string->number)
            ("min-height" min-height db-string->number)
            ("max-height" max-height db-string->number)
            ("bytes" (min-bytes max-bytes) db-string->number)
            ("min-bytes" min-bytes db-string->number)
            ("max-bytes" max-bytes db-string->number)

            ("trunc-display" truncation-display-action intern)
            ("truncation-display-action" truncation-display-action intern)
            ("padding-action" padding-action intern)
            ("right-justify" padding-action (lambda (x) 'db-pad-left))
            ("actual->display" actual->display intern)
            ("a->d" actual->display intern)
            ("display->actual" display->actual intern)
            ("d->a" display->actual intern)

            ;; match-actual->display and match-display->actual,
            ;; fields 13 and 14  [??? --ttn]

            ("truncation-editing-action" truncation-editing-action intern)
            ("trunc-edit" truncation-editing-action intern)
            ("reachable" reachablep (lambda (x) t))
            ("unreachable" reachablep (lambda (x) nil)))))


(defun db-pad-left (min-width rep rep-length)
  (concat (make-string (- min-width rep-length) 32) rep))

(defun db-pad-right (min-width rep rep-length)
  (concat rep (make-string (- min-width rep-length) 32)))


(defun db-callconvert (convert fieldtext &rest args)
  (let ((res (if convert
                 (condition-case err
                     ;; Try calling it with one arg.
                     (funcall convert fieldtext)
                   (wrong-number-of-arguments
                    ;; Call it with all args.
                    (apply convert fieldtext args))
                   (error
                    ;; Otherwise resignal; "while t" makes this work
                    ;; under the debugger (see, eg, the code for the
                    ;; "error" function).
                    (while t
                      (signal (car err) (cdr err)))))
               fieldtext)))
    (if (= 3 (length args))
        ;; display->actual
        res
      ;; actual->display
      (if (stringp res)
          res
        "<ERROR>"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;

;; If the user has deleted some of the leading spaces, they'll be restored.
;; Don't do anything about tabs, not even untabifying.

(defun db-unindentify (text)
  (let ((amt (dbf-this-field-indent)))
    (if amt
        (replace-regexp-in-string
         (concat "\n" (db-space-maybe-rx amt)) "\n" text)
      text)))

(defun db-space-maybe-rx (n)
  ;; Return a regexp matching N or fewer occurrences of the space character.
  ;; If N is nil, return the empty string, which is sometimes not a regexp you
  ;; want to search for by itself.
  (if n
      (if (> n
             ;; Emacs 19's regexp routines fix bugs in the Emacs 18 and Lucid
             ;; Emacs versions, but are sometimes much slower.  For deeply
             ;; indented fields, this can result in very slow editing.  We
             ;; disable some error-checking and correction for fields indented
             ;; more than 8 characters.
             8)
          (make-string (or n 0) 32)
        (let ((result (make-string (* 2 n) 32)))
          (setq n (1- (* 2 n)))
          (while (> n 0)
            (aset result n ??)
            (decf n 2))
          result))
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode selection
;;;


(defvar database-view-mode-menu)        ; fixme: document. --ttn
(defvar database-edit-mode-menu)        ; fixme: document. --ttn

(defalias 'database-view-mode 'db-view-mode) ; for C-h m
(defun db-view-mode (&optional arg)
  "Switch to Database View mode.
With an argument, toggle between Database View and Database Edit modes."
  (interactive "P")

  (cond ((and arg (eq 'database-view-mode major-mode))
         (db-edit-mode))
        ;; If already in Database View mode, don't do anything.
        ((not (eq 'database-view-mode major-mode))
         (dbf-process-field-maybe t)
         (setq major-mode 'database-view-mode
               mode-name "Database View")
         (use-local-map database-view-mode-map)
         (when (edb--rget '(:edb1 :bprops) (current-buffer)) ; hmmm
           (edb--S! :this-fidx nil)
           (edb--S! :this-ds nil))
         (setq buffer-read-only t)
         (goto-char (point-min))
         (dbf-set-this-field-modified-p nil)
         (easy-menu-remove database-edit-mode-menu)
         (easy-menu-add database-view-mode-menu)
         (edb--1run-hooks 'db-view-mode-hooks)
         (force-mode-line-update))))

(defalias 'database-edit-mode 'db-edit-mode) ; for C-h m
(defun db-edit-mode (&optional arg)
  "Switch to Database Edit mode.
With an argument, toggle between Database Edit and Database View modes."
  (cond ((not (db-data-display-buffer-p))
         (error "Only call this in database mode."))
        ((and arg (eq 'database-edit-mode major-mode))
         (db-view-mode))
        (t
         (setq major-mode 'database-edit-mode
               mode-name "Database Edit")
         (use-local-map database-edit-mode-map)
         (if (edb--1D dbc-database :modifiable-p)
             (setq buffer-read-only nil)
           (message
            "%s" (substitute-command-keys
                  (concat "Database is not modifiable; "
                          "change that with \\[db-toggle-modifiable-p]"))))
         (easy-menu-add database-edit-mode-menu)
         (easy-menu-remove database-view-mode-menu)
         (edb--1run-hooks 'db-edit-mode-hooks)
         (force-mode-line-update))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement in the format
;;;

(defun db-parse-buffer-error (format-string &rest args)
  ;;(debug nil (apply 'format format-string args))
  (db-emergency-restore-format)
  (db-message "%s; %s"
              "I was confused about where I was"
              "changes to the field might have been lost"))

(defun db-next-line-or-field (arg)
  "Move to ARGth next line.  If that would move out of the current field,
move to the closest field, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
        goal-line)
    ;; Determine goal line.
    (db-forward-line-wrapping arg)
    (db-jump-to-point)
    (setq goal-line (db-current-line))
    ;; Move to proper column.
    (move-to-column goal-column)
    (db-jump-to-point)
    ;; Off goal line: move back and as near to the goal column as possible.
    (when (> (db-current-line) goal-line)
      (db-previous-field-internal 1)
      (goto-char (dbf-this-field-end-pos)))))

(defun db-move-to-field-exact (arg)
  "Move to the ARGth field in the display.  Ignores reachablep."
  (db-first-field-internal t)
  (db-next-field-internal arg t)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-next-field (arg)
  "Move to ARGth next reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char (edb--S :fbeg))
  (if (> arg 0)
      (db-next-field-internal arg)
    (db-previous-field-internal (- arg)))
  ;; We have just moved to a new field, which certainly isn't modified yet.
  (dbf-set-this-field-modified-p nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-next-field-internal (arg &optional exact)
  ;; Arg should be positive.  Assumes point is at the beginning of the field.
  ;; If EXACT is non-nil, reachablep is ignored.
  (let* ((displayspecs (edb--S :displayspecs))
         (iftxt (edb--S :iftxt))
         (this-fidx (edb--S :this-fidx))
         (shown (edb--S :shown))
         (len (length displayspecs)))
    (while (> arg 0)
      (if (not (db-skip-string-forward (aref shown this-fidx)))
          (db-parse-buffer-error
           "Didn't find field %s text `%s'."
           this-fidx (aref shown this-fidx))
        (edb--S! :this-fidx (incf this-fidx))
        (decf arg)
        (when (= len this-fidx)
          (unless (db-skip-string-forward (aref iftxt len))
            (db-parse-buffer-error
             "Didn't find trailing text `%s' after field %s."
             (aref iftxt len)
             (1- len)))
          (edb--S! :this-fidx (setq this-fidx 0))
          (goto-char (point-min)))
        (unless (db-skip-string-forward (aref iftxt this-fidx))
          (db-parse-buffer-error
           "Didn't find field separator `%s' before field %s."
           (aref iftxt this-fidx) this-fidx))
        ;; Implement reachablep.
        ;; fixme: handle infinite loop. --ttn
        (unless (or exact (edb--1ds-reachablep
                           (aref displayspecs this-fidx)))
          (incf arg))))
    (edb--S! :this-ds (aref displayspecs this-fidx))
    (edb--S! :fbeg (point)))
  (buffer-disable-undo)
  (buffer-enable-undo)

  (when (looking-at (regexp-quote (aref (edb--S :shown)
                                        (edb--S :this-fidx))))
    (let ((end-of-match (match-end 0)))
      (set-marker (edb--S :fend)
                  (if (= end-of-match (point-max))
                      nil
                    (1+ end-of-match))
                  (current-buffer)))))

(defun db-previous-line-or-field (arg)
  "Move to ARGth previous line.  If that would move out of the current field,
move to the closest field, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
        (vacated-line (db-current-line))
        this-line)
    (db-forward-line-wrapping (- arg))
    (move-to-column goal-column)
    (db-jump-to-point)
    (setq this-line (db-current-line))
    (when (= this-line vacated-line)
      ;; We moved to a line containing no field, so db-jump-to-point
      ;; put us in the field following point; ie, one on the line in
      ;; which we started.  This is not the desired behavior.
      ;; Get to a line containing a field.
      (db-previous-field-internal 1)
      (goto-char (dbf-this-field-end-pos))
      ;; Go to the correct column.
      (move-to-column goal-column)
      ;; Avoid getting dumped back into this field.
      (goto-char (min (point) (dbf-this-field-end-pos)))
      ;; And end up there.
      (db-jump-to-point))))

(defun db-previous-field (&optional arg)
  "Move to ARGth previous reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char (edb--S :fbeg))
  (if (> arg 0)
      (db-previous-field-internal arg)
    (db-next-field-internal (- arg)))
  (dbf-set-this-field-modified-p nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-previous-field-internal (arg)
  ;; Arg should be positive.  Assume point is at the beginning of the field.
  ;; pb = previous inter-field-text beginning
  (let* ((displayspecs (edb--S :displayspecs))
         (iftxt (edb--S :iftxt))
         (this-fidx (edb--S :this-fidx))
         (shown (edb--S :shown))
         (len (length displayspecs))
         (pb (marker-position (edb--S :fend))))
    (if pb
        (decf pb))
    (while (> arg 0)
      (unless (db-skip-string-backward (aref iftxt this-fidx))
        (db-parse-buffer-error
         "Didn't find field separator `%s' before field %s."
         (aref iftxt this-fidx) this-fidx))
      (setq pb (point))
      (edb--S! :this-fidx (decf this-fidx))
      (decf arg)
      (when (< this-fidx 0)
        (edb--S! :this-fidx (setq this-fidx (1- len)))
        (goto-char (point-max))
        (if (db-skip-string-backward (aref iftxt len))
            (setq pb (point))
          (db-parse-buffer-error
           "Didn't find trailing text `%s' after field %s."
           (aref iftxt len) this-fidx)))
      (unless (db-skip-string-backward (aref shown this-fidx))
        (db-parse-buffer-error
         "Didn't find field %s text `%s'."
         this-fidx (aref shown this-fidx)))
      ;; Implement reachablep.
      ;; fixme: handle infinite loop. --ttn
      (unless (edb--1ds-reachablep (aref displayspecs this-fidx))
        (incf arg)))
    (edb--S! :this-ds (aref displayspecs this-fidx))
    (edb--S! :fbeg (point))
    (buffer-disable-undo)
    (buffer-enable-undo)
    (set-marker (edb--S :fend)
                (and pb
                     (if (or (= 1 pb)
                             (= (point-max) pb))
                         nil
                       (1+ pb))))))

(defun db-first-field-internal (&optional exact)
  ;; Move to first field.  Optional EXACT means ignore reachability.
  (if (edb--S :this-fidx)
      (dbf-process-field-maybe t)
    (db-edit-mode))
  (edb--S! :this-fidx 0)
  ;; We need this even if field-index was nil, because someone might have
  ;; sneakily moved point.  (In fact, this is called after point is moved
  ;; via mouse.)
  (goto-char (point-min))
  (let ((iftxt (edb--S :iftxt))
        (this-fidx (edb--S :this-fidx)))
    (unless (db-skip-string-forward (aref iftxt 0))
      (db-parse-buffer-error
       "Didn't find field separator `%s' before field %s."
       (aref iftxt this-fidx) this-fidx))
    (db-next-field-internal 0)
    ;; Implement reachablep.
    (unless (or exact
                (edb--1ds-reachablep
                 (aref (edb--S :displayspecs) this-fidx)))
      (db-next-field-internal 1)))
  (dbf-set-this-field-modified-p nil))

(defun db-first-field ()
  "Move to first field."
  (interactive)
  (db-first-field-internal nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-last-field ()
  "Move to last field."
  (interactive)
  (db-first-field-internal nil)
  (db-previous-field 1))

(defun db-scroll-up ()
  "Like scroll-up, but also edits the nearest database field."
  (interactive)
  (scroll-up)
  (db-jump-to-point t))

(defun db-scroll-down ()
  "Like scroll-down, but also edits the nearest database field."
  (interactive)
  (scroll-down)
  (db-jump-to-point t))

(defun db-jump-to-point (&optional quietly)
  "In a data display buffer, move to the field containing or following point.
In a summary buffer, move to the record displayed around point."
  (cond ((db-data-display-buffer-p)
         (let* ((this-fidx (edb--S :this-fidx))
                (beg (and this-fidx (edb--S :fbeg))))
           (unless (and beg (and (<= beg (point))
                                 (<= (point) (dbf-this-field-end-pos))))
             ;; moving outside current field.
             (let ((new-point (point)))
               (set-marker (edb--G :1moving-mark) (point))
               ;; Go back to where we were:
               ;; if we were in a field, get back in it.
               (when this-fidx
                 (goto-char beg))
               (if (and this-fidx
                        (> (marker-position (edb--G :1moving-mark)) (point)))
                   ;; We are in a field and moving forward.
                   (progn
                     (dbf-process-field-maybe t)
                     (goto-char beg))
                 (db-first-field-internal nil))
               ;; If the dbf-process-field-maybe redisplays the entire record,
               ;; the marker gets wiped out (points to the beginning of the
               ;; buffer, because the buffer is cleared and refilled).
               (let ((moving-pos (marker-position (edb--G :1moving-mark))))
                 (unless (= 1 moving-pos)
                   (setq new-point moving-pos)))
               (set-marker (edb--G :1moving-mark) nil)
               (let ((len (length (edb--S :displayspecs))))
                 (while (and (> new-point (dbf-this-field-end-pos))
                             (< (edb--S :this-fidx) (1- len)))
                   ;; The EXACT argument is t so we don't infinite-loop when
                   ;; the last field is unreachable.
                   (db-next-field-internal 1 t)))
               (let ((this-ds (edb--S :this-ds)))
                 (unless (edb--1ds-reachablep this-ds)
                   ;; This message is getting wiped out by the
                   ;; mouse-button-up event.  How can I fix this?
                   ;; Hint:  Transposing the following two statements is
                   ;; not the answer.
                   (unless quietly
                     (db-message "Field `%s' is unreachable"
                                 (db-fname<-fno
                                  (edb--1ds-record-index this-ds)
                                  dbc-database)))
                   (db-next-field-internal 1)))

               (edb--1run-hooks 'dbf-enter-field-hook)
               ;; The max makes sure we're in a field, not beyond it.
               ;; The min is there only for the last field (because we could
               ;; be past it, in which case there's not a following field).
               (goto-char (min (max new-point (edb--S :fbeg))
                               (dbf-this-field-end-pos)))))
           ;; Check not in indentation even if didn't move to a new field.
           (when (let ((amt (dbf-this-field-indent)))
                   (and amt
                        (> amt 0)
                        (db-looking-back-at "^ +")
                        (< (current-column) amt)))
             (db-beginning-of-line-or-field))))
        ((db-summary-buffer-p)
         ;; This is wrong in the presence of hidden directory lines.
         (beginning-of-line)
         (let* ((p (edb--S :point))
                (lines (count-lines p (point)))
                (signed (if (< p (point)) lines (- lines))))
           (goto-char p)
           (dbs-next-record-ignore-hiding
            (/ signed (edb--1D dbc-database :sum1lines)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement within a field
;;;

;; These shouldn't be called if not on a field, so they don't check.

(defsubst dbf-this-field-end-pos ()
  (let ((pos (marker-position (edb--S :fend))))
    (if pos
        (1- pos)
      (point-max))))

(defun dbf-this-field-indent ()
  (let ((in (edb--1ds-indent (edb--S :this-ds))))
    (and in (if (numberp in)
                in
              (save-excursion
                (goto-char (edb--S :fbeg))
                (current-column))))))

;;;
;;; Checking
;;;

(defalias 'dbf-inform-outside-field
  ;; Function to call when point attempts to leave a field.
  ;; It should take one argument, a short message string.
  'error)

(defsubst dbf-check-if-outside-field (&optional quietly)
  ;; Move point to beginning of field if it's before that.
  (let ((beg-pos (edb--S :fbeg)))
    (when (< (point) beg-pos)
      (goto-char beg-pos)
      (unless quietly
        (db-message "Beginning of field"))))
  ;; Move point to end of field if it's beyond that.
  (let ((end-pos (dbf-this-field-end-pos)))
    (when (> (point) end-pos)
      (goto-char end-pos)
      (unless quietly
        (dbf-inform-outside-field "End of field.")))))


;;;
;;; Movement
;;;

(defsubst db-beginning-of-field ()
  "Move to the beginning of the current field."
  (interactive)
  (goto-char (edb--S :fbeg)))

(defsubst db-end-of-field ()
  "Move to the end of the current field."
  (interactive)
  (goto-char (dbf-this-field-end-pos)))

(defun db-beginning-of-line-or-field ()
  "Move to the beginning of the current line of the current field.
If invoked twice in succession, move to beginning of field."
  (interactive)
  (if (eq 'db-beginning-of-line-or-field last-command)
      (db-beginning-of-field)
    (beginning-of-line)
    (db-skip-regexp-forward (db-space-maybe-rx (dbf-this-field-indent)))
    (dbf-check-if-outside-field t)))

(defun db-end-of-line-or-field (arg)
  "Move to the end of the current line of the current field.
If invoked twice in succession, move to end of field."
  (interactive "p")
  (if (eq 'db-end-of-line-or-field last-command)
      (db-end-of-field)
    (end-of-line arg)
    (dbf-check-if-outside-field t)))

(defun db-forward-char (arg)
  "Like forward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-backward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
        (if (eobp)
            ;; This is so we get the error "End of field"
            ;; instead of "End of buffer".
            (progn
              (setq arg 0)
              (dbf-inform-outside-field "End of field."))
          (forward-char 1)
          (db-skip-regexp-forward (concat "^" (db-space-maybe-rx indent)))
          (decf arg)))
      (dbf-check-if-outside-field))))

(defun db-backward-char (arg)
  "Like backward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-forward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
        (if (bobp)
            ;; This is so we get the error "Beginning of field"
            ;; instead of "Beginning of buffer".
            (progn
              (setq arg 0)
              (dbf-inform-outside-field "Beginning of field."))
          (let ((rx (concat "^" (db-space-maybe-rx indent)))
                (here (point)))
            (when (re-search-backward rx nil t)
              (if (= here (match-end 0))
                  t
                (goto-char here)
                nil)))
          (backward-char 1)
          (decf arg)))
      (dbf-check-if-outside-field))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun db-delete-char (arg)
  (interactive "p")
  "Like delete-char, but won't delete outside the field."
  (delete-region (point) (progn (db-forward-char arg) (point))))

(defun db-backward-delete-char (arg)
  (interactive "p")
  "Like delete-backward-char, but won't delete outside the field."
  (delete-region (point) (progn (db-backward-char arg) (point))))

(defun db-forward-word (arg)
  "Like forward-word, but won't go outside field."
  (interactive "p")
  (forward-word arg)
  (dbf-check-if-outside-field))

(defun db-backward-word (arg)
  "Like backward-word, but won't go outside field."
  (interactive "p")
  (db-forward-word (- arg)))

(defun db-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (let ((text (db-unindentify (buffer-substring beg end))))
    (if (eq last-command 'db-kill-region)
        (kill-append text (< end beg))
      (push text kill-ring)
      (when (> (length kill-ring) kill-ring-max)
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))))
  (setq this-command 'db-kill-region)
  (setq kill-ring-yank-pointer kill-ring))

(defun db-kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
See `kill-region' for details."
  (interactive "*r")
  (db-copy-region-as-kill beg end)
  (delete-region beg end))

(defun db-kill-word (arg)
  "Like kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-region (point) (progn (db-forward-word arg) (point))))

(defun db-backward-kill-word (arg)
  "Like backward-kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-word (- arg)))

(defun db-kill-line (arg)
  "Like kill-line, but won't delete outside the field."
  (interactive "p")
  (let ((here (point)))
    (db-end-of-line-or-field arg)
    (when (< (point) (dbf-this-field-end-pos))
      (let ((indent (dbf-this-field-indent)))
        (cond (kill-whole-line
               (db-skip-regexp-forward
                (concat "[ \t]*\n" (db-space-maybe-rx indent))))
              ((and (eolp) (= indent (current-column)))
               (forward-char (1+ indent))))))
    (db-kill-region here (point))))

(defun db-kill-to-end ()
  "Kill from point to the end of the current field."
  (interactive)
  (db-kill-region (point) (dbf-this-field-end-pos)))

(defun db-newline (arg)
  "Insert a newline.  Will not make the current field too tall.
If current field's maximum height is 1 line, move to the next field instead."
  (interactive "p")
  ;; ignores the argument
  (let ((max-h (edb--1ds-max-height (edb--S :this-ds))))
    (if (or (not max-h)
            (< (count-lines (edb--S :fbeg) (dbf-this-field-end-pos))
               max-h))
        (let ((indent (dbf-this-field-indent)))
          (newline 1)
          (when indent (insert (make-string indent 32))))
      (if (= 1 max-h)
          (db-next-field 1)
        (db-message "Field is at maximum height already")))))

(defun db-open-line (arg)
  "Insert a newline and leave point before it.
Will not make the current field too tall."
  (interactive "p")
  (let ((here (point)))
    (db-newline arg)
    (goto-char here)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value processing for fields and records
;;;

(defun dbf-process-field-maybe (set-text-p)
  ;; Set the value of the current record from the current field.
  ;; If arg SET-TEXT-P is non-nil, update the display as well.
  ;; Return t if field is unmodified or text is OK; nil otherwise.
  ;; May move point.
  (let ((fno (and (edb--rget '(:edb1 :bprops) (current-buffer))
                  (edb--S :this-fidx)))
        (shown (edb--S :shown)))
    (cond
     ((not fno))
     ((not (dbf-this-field-modified-p)))
     ((or (< (point) (edb--S :fbeg))
          (> (point) (dbf-this-field-end-pos)))
      (db-parse-buffer-error
       "Point was outside (%d) of current field (%d - %d)."
       (point) (edb--S :fbeg) (dbf-this-field-end-pos))
      ;; Return something in case `db-parse-buffer-error' returns.
      ;; TODO: Is this the right return value? --ttn
      nil)
     ((equal (dbf-this-field-text) (aref shown fno))
      ;; Field is unchanged, so mark it unmodified.
      (dbf-set-this-field-modified-p nil))
     (t
      ;; Field has been modified.
      (let* ((this-ds (edb--S :this-ds))
             (this-fname (dbf-this-field-name this-ds))
             (cur (db-callconvert
                   (edb--1ds-display->actual this-ds)
                   (let ((text (dbf-this-field-text)))
                     (db-unindentify text))
                   (aref (dbf-displayed-record) fno)
                   (dbf-displayed-record)
                   fno))
             (idx (edb--1ds-record-index this-ds))
             (old (aref (dbf-displayed-record) idx))
             (under (edb--S :under))
             (saved-modified-p (edb--S :utkmodp)))
        (unless (equal cur old)
          ;; The new value is different from the old.
          (dbf-set-this-record-modified-p t)
          (db-check-constraint cur under idx dbc-database)
          (aset under idx cur)

          (when set-text-p
            (aset shown fno
                  (let ((pr (db-ds-printed this-ds under))
                        (in (edb--1ds-indent this-ds)))
                    (if in
                        (if (numberp in)
                            (replace-regexp-in-string
                             "\n"
                             (concat "\n" (make-string in 32))
                             pr)
                          ;; Why can't I use (dbf-this-field-indent) even here?
                          (if (db-find-char ?\n pr)
                              (error "Don't know how much to indent.")
                            pr))
                      pr))))
          ;; No need to do redisplay before the change-hooks are
          ;; called since the user's version is already onscreen
          ;; and that will be very similar indeed to the display
          ;; text.
          (unless saved-modified-p
            (setq dbf-redisplay-entire-record-p
                  (or (and dbf-first-change-function
                           (funcall dbf-first-change-function
                                    this-fname old cur))
                      dbf-redisplay-entire-record-p)))
          (setq dbf-redisplay-entire-record-p
                (or (and dbf-every-change-function
                         (funcall dbf-every-change-function
                                  this-fname old cur))
                    dbf-redisplay-entire-record-p))
          (setq dbf-redisplay-entire-record-p
                (let ((change-function (aref (edb--S :change-functions) idx)))
                  (or (and change-function
                           (funcall change-function
                                    this-fname old cur))
                      dbf-redisplay-entire-record-p))))
        ;; The text is different; the value may or may not have differed.
        ;; Display the standard representation for this value, which has
        ;; already been computed.
        (when set-text-p
          (unless (dbf-redisplay-entire-record-maybe)
            ;; set-field-text always returns nil
            (dbf-set-this-field-text
             (aref shown fno))))
        (dbf-set-this-field-modified-p t))))))


(defun dbf-redisplay-entire-record-maybe ()
  ;; If `dbf-redisplay-entire-record-p' is non-nil, redisplay current record
  ;; and return t; otherwise return nil.
  (when dbf-redisplay-entire-record-p
    (setq dbf-redisplay-entire-record-p nil)
    (db-emergency-restore-format t)
    t))


(defun dbf-process-current-record-maybe (set-text-p)
  ;; Commit changes to the record being displayed and edited.  If the current
  ;; record (see `dbf-displayed-record') is a modified copy of a database
  ;; record, this copies it back to the original database record, modifying
  ;; the database by side effect.  Return t if successful, nil otherwise.
  ;; SET-TEXT-P non-nil means to also update the display.
  (when (edb--S :index)
    ;; Sets the field unmodified, if appropriate
    (dbf-process-field-maybe set-text-p)
    (when (edb--S :utkmodp)
      ;; Do any programmer-requested checking or postprocessing here.
      ;; This function may err, aborting out of whatever was trying to
      ;; process the current record and do something else.
      (edb--1run-hook-with-arg 'dbf-after-record-change-function
                               (dbf-displayed-record))
      (db-copy-r2r (edb--S :under) (edb--S :original))
      (dbf-update-summary-item (edb--S :index))
      ;; what about hiddenp and markedp? --ttn
      (database-set-modified-p dbc-database t)
      (edb--S! :utkmodp nil)
      (dbf-set-this-field-modified-p nil))
    ;; This function shouldn't have been called on a non-database record; how
    ;; did we get here?  It may not be the case that the info is about to be
    ;; abandoned.
    (or (not (edb--S :utkmodp))
        (y-or-n-p "Abandon the displayed information? ")
        (error "Don't abandon displayed information."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undoing changes
;;;

(defun db-revert-field (&optional quietly)
  "Undo any changes made since entering this field.
Replace the onscreen text in this field with that of the underlying record.

A similar effect can be had by invoking \\[advertised-undo] multiple times."
  (interactive)
  (let ((fname (unless quietly (dbf-this-field-name (edb--S :this-ds)))))
    (if (dbf-this-field-modified-p)
        (progn
          (dbf-set-this-field-text
           (aref (edb--S :shown) (edb--S :this-fidx)))
          (dbf-set-this-field-modified-p nil)
          (unless quietly
            (db-message "Reverted field `%s'" fname)))
      (unless quietly
        (db-message "Can't revert field `%s'; no changes since moving onto it"
                    fname)))))

(defun db-revert-record ()
  "Set the record to be the same as the corresponding one in the database.
In other words, undo any changes made since entering this record."
  (interactive)
  (db-revert-field t)
  (if (edb--S :utkmodp)
      (let ((buffer-read-only nil))
        (edb--S! :utkmodp nil)
        (db-display-record (dbf-displayed-record) t)
        (let ((this-fidx (edb--S :this-fidx)))
          (when this-fidx
            (db-move-to-field-exact this-fidx)))
        (db-message "Reverted record"))
    (db-message "Can't revert this record; no changes since selecting it")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set displayspec from string
;;;


(defun db-dspec<-dtype (type)
  "Return a copy of the displayspec corresponding to string or symbol TYPE.
Return nil if there's no corresponding displayspec."
  (let ((ds (gethash (if (stringp type)
                         (intern type)
                       type)
                     (edb--G :1displaytypes))))
    (when ds (if (symbolp ds)
                 (db-dspec<-dtype ds)
               (edb--copy-1ds ds)))))


(defun db-dspec<-string (s db)
  ;; Assume match-data from `db-ds+opts-rx'.  S is nil if from the buffer.
  (let* ((ls (split-string (match-string 1 s) ","))
         (fname (intern (car ls)))
         (fidx (and db (gethash fname (edb--1D db :nm2no))))
         (type (and fidx (aref (db-rs-slice db 'edb--1rs-type) fidx)))
         ds)
    (when (and db (not fidx))
      (error "%s is not a field or field abbreviation."
             fname))
    (setq ds (db-dspec<-type/opts type (cdr ls)))
    (unless ds
      (error "Type %s in field %d (%s) not recognized."
             type
             fname fidx))
    (setf (edb--1ds-record-index ds) fidx)
    ds))


(defun db-dspec<-type/opts (type opts &optional notype-ok)
  ;; Either TYPE or OPTS (list of strings) must specify a type, unless
  ;; optional argument NOTYPE-OK is specified, in which case an empty
  ;; displayspec may be returned.

  ;; Ordinarily (for instance, when this is being called to parse part of a
  ;; format), NOTYPE-OK should not be specified, so that invalid
  ;; displaytypes aren't created.

  ;; A type in OPTS overrides TYPE.
  (if (not (setq opts (delete "" opts)))
      (if type
          (or (db-dspec<-dtype type)
              (error "No such displaytype as `%s'." type))
        (edb--make-1ds))
    (let (ds)
      ;; set the displayspec
      ;; note tricky sequencing
      (if (setq ds (db-dspec<-dtype (intern (car opts))))
          (pop opts)
        (if type
            (setq ds (db-dspec<-dtype type))
          (error "No type specified in `%s'." opts)))

      (while opts
        (let* ((pair (split-string (pop opts) "="))
               (opt (car pair))
               (val (or (cadr pair) ""))
               (spec (or (assoc opt db-optspec-list)
                         (error "Invalid optional field spec name or type: %s"
                                opt))))
          (funcall (nth 1 spec) ds (funcall (nth 2 spec) val))))
      ds)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a format file
;;;

(defun db-setup-data-display-buffer (template db new-p)
  ;; Create and return a data display buffer.  This is only called when a
  ;; brand-new data display buffer is being created, not when one is being
  ;; refreshed.  Arguments are TEMPLATE DB NEW-P.  TEMPLATE may either be
  ;; a buffer containing a display format, or the name of a format file.
  ;; If NEW-P is non-nil, then the database's auxiliary file is read and
  ;; its field variables are set.
  ;;
  ;; WARNING: If the format file's local variables set particular database
  ;; slots (such as fieldnames), and NEW-P is nil, then the database may be
  ;; left in an inconsistent state.  The "primary" format, which is read in
  ;; before the database is, should perform any such neccessary actions.
  (unless (bufferp template)
    (setq template (expand-file-name template))
    (unless (file-readable-p template)
      (error "Can't read format file `%s'." template)))

  (with-current-buffer (db-make-data-display-buffer db new-p)
    (assert (eq dbc-database db) t)
    (setq buffer-read-only nil)         ; (database-mode) set it to t

    (funcall (cond ((bufferp template)
                    'insert-buffer-substring)
                   (t
                    'insert-file-contents))
             template)
    (edb--S! :format-file (if (bufferp template)
                              "(internal)"
                            template))

    (when (and new-p (not (equal "(internal)" (edb--S :format-file))))
      (let ((aux (db-locate-readable-file-prefer-cwd
                  (file-name-sans-extension (edb--1D db :file))
                  (cons default-directory db-aux-file-path)
                  db-aux-file-suffixes)))
        (when aux
          ;; Note that the variable `database' is dynamically bound.
          (let ((database db))
            (load-file aux)))))
    ;; Note that the variable `database' is dynamically bound.
    (let ((database db))
      (db-really-hack-local-variables))

    ;; This is the second half of `insert-buffer-substring-no-properties'
    ;; that we used to use.  Unfortunately, we need the properties to be
    ;; around during local variables processing (ugh).
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil))

    ;; Initialize local variables.
    ;; Assume `dbc-database' already set.
    (let ((nfields (length (database-fieldnames dbc-database))))
      (unless (< 0 nfields)
        (error "Can't tell how many fields database has."))
      (edb--S! :change-functions (make-vector nfields nil))
      (edb--S! :under (make-vector nfields nil))

      (when new-p
        ;; Initialize database variables.  We didn't do this earlier because
        ;; they may depend on some values set in the format file.
        (unless (edb--1D db :togp)
          ;; Do this before the format is parsed but after the format's local
          ;; variables have been hacked.
          (unless (database-field-priorities db)
            (setf (database-field-priorities db)
                  (list (mapcar 'list (number-sequence 0 (1- nfields)))))))))

    (db-setup-ddb-parse-displayspecs db)

    (setq buffer-read-only t)
    (current-buffer)))

(defun db-make-data-display-buffer (db new-p)
  ;; Create and return a data display buffer; set some vars.
  (with-current-buffer (create-file-buffer (edb--1D db :file))
    (set (make-local-variable 'edb--bprops)
         (edb--rinit '(:edb1 :bprops) (current-buffer)
                     :size 23 :test 'eq :weakness 'key))
    (edb--S! :fend (make-marker))
    (edb--S! :wraparound 'delay)
    (edb--S! :stay-in-edit-mode-p t)
    (let ((dir (file-name-directory (edb--1D db :file))))
      (when dir
        (setq default-directory (expand-file-name dir))))
    (set (make-local-variable 'dbc-database) db)
    (dolist (var '(db-new-record-function
                   dbf-set-this-record-modified-function
                   dbf-redisplay-entire-record-p
                   dbf-before-display-record-function
                   dbf-enter-field-hook
                   dbf-first-change-function
                   dbf-every-change-function
                   dbf-after-record-change-function
                   dbf-format-name-spec-alist))
      (set (make-local-variable var) nil))
    (unless new-p
      ;; These are per-data-display-buffer variables.
      (let ((nfields (length (database-fieldnames db))))
        (edb--S! :change-functions (make-vector nfields nil))
        (edb--S! :under (make-vector nfields nil))))
    (database-mode)
    (current-buffer)))


(defun db-setup-ddb-parse-displayspecs (db)
  ;; Get rid of local variables.
  (goto-char (point-max))
  (search-backward "\n\^L" (point-min) 'move)
  (when (search-forward
         (concat "Local"                ; use `concat' to avoid
                 " "                    ; misinterpretation
                 "Variables:")
         nil t)
    (beginning-of-line 1)
    (delete-region (point) (point-max)))
  ;; Get rid of whitespace at end of buffer.
  (goto-char (point-max))
  (re-search-backward "[^ \t\n]")
  (delete-region (match-end 0) (point-max))
  ;; Get rid of whitespace at ends of lines.
  (goto-char (point-min))
  (while (re-search-forward  "[ \t]+$" nil t)
    (delete-region (match-beginning 0) (match-end 0)))

  (let ((prev-end (point-min))
        (backslash-placeholder (and (goto-char (point-min))
                                    (search-forward "\\\\" nil t)
                                    ;; assume this doesn't return nil
                                    (db-unused-char-in-buffer)))
        len
        beginning end ds
        ds-ls
        ift-ls)                         ; inter-field-text

    (when backslash-placeholder
      (setq backslash-placeholder (char-to-string backslash-placeholder))
      (goto-char (point-min))
      (while (search-forward "\\\\" nil t)
        (replace-match backslash-placeholder nil t)))

    (edb--S! :default-sumfmt nil)

    (goto-char (point-min))
    (while (re-search-forward db-ds+opts-rx nil t)
      (setq beginning (match-beginning 0)
            end (match-end 0)
            ;; Call "internal" version of function because match-data is set.
            ;; nil as first argument means make it from the buffer.
            ds (db-dspec<-string nil db))

      ;; Fix up backslash-replacement.  The buffer is fixed up instead of
      ;; just the ift-ls because of the call to current-column.
      (when backslash-placeholder
        (save-excursion
          (save-restriction
            (narrow-to-region prev-end beginning)
            (goto-char prev-end)
            (while (search-forward backslash-placeholder nil t)
              (replace-match "\\" nil t)))))

      (push (buffer-substring prev-end beginning) ift-ls)
      ;; Match about to be deleted; we just used the old value.
      (setq prev-end beginning)

      (unless (edb--S :default-sumfmt)
        (edb--S! :default-sumfmt (save-excursion
                                   (buffer-substring
                                    (progn (beginning-of-line 1)
                                           (point))
                                    (progn (end-of-line 1)
                                           (point)))))
        (unless (edb--S :sumfmt)
          (edb--S! :sumfmt (edb--S :default-sumfmt))))

      (delete-region beginning end)

      (when (eq t (edb--1ds-indent ds))
        (setf (edb--1ds-indent ds) (current-column)))

      (push ds ds-ls))

    ;; Fix up backslash-replacement for the post-last text.
    (when backslash-placeholder
      (save-excursion
        (save-restriction
          (narrow-to-region prev-end (point-max))
          (goto-char prev-end)
          (while (search-forward backslash-placeholder nil t)
            (replace-match "\\" nil t)))))

    (push (buffer-substring prev-end (point-max)) ift-ls)

    (edb--S! :iftxt (vconcat (nreverse ift-ls)))
    (edb--S! :displayspecs (vconcat (nreverse ds-ls)))
    (setq len (length (edb--S :displayspecs)))
    ;; The vector for :search-defaults is one element longer than the number
    ;; of fields; last element is the default for a search over all fields.
    (edb--S! :search-defaults (make-vector (1+ len) nil))
    (edb--S! :shown (make-vector len nil)))

  ;; Initialize more local variables.
  (edb--S! :fidx2dsidx (make-vector (length (database-fieldnames db)) nil))
  (let ((all-ds (edb--S :displayspecs))
        (fidx2dsidx (edb--S :fidx2dsidx)))
    (dotimes (fsno (length all-ds))
      (aset fidx2dsidx (edb--1ds-record-index (aref all-ds fsno)) fsno)))

  (dbf-set-summary-format
   (or (edb--S :sumfmt)
       (mapconcat (lambda (sym)
                    (format "\\%s" sym))
                  (append (database-fieldnames db) nil)
                  " ")))

  (set-buffer-modified-p nil))


(defun db-additional-data-display-buffer ()
  "Create another data display buffer in which to view this database."
  (interactive)
  (dbf-process-current-record-maybe t)
  (let* ((cur (current-buffer))
         (database dbc-database)
         (new (db-make-data-display-buffer database nil)))
    (let ((ddbufs (edb--1D database :ddbufs)))
      (edb--1D! database :ddbufs (cons new ddbufs)))
    (switch-to-buffer-other-window new)
    (db-copy-buffer-local-variables cur)
    (edb--rput '(:edb1 :bprops) (current-buffer)
               (copy-hash-table (edb--rget '(:edb1 :bprops) cur)))
    (edb--S! :sumbuf nil)
    ;; Here are the trampled-on variables that we really cared about.
    (edb--S! :under (make-vector (length (edb--S :original)) nil))
    (db-emergency-restore-format t)))


(defun db-change-format (&optional format-name filename)
  "Select and use an alternate display format to view the database.
If neither FORMAT-NAME nor FILENAME is specified (as is the case when this
is called interactively), the user is prompted for them.  In Emacs Lisp
code, if `dbf-format-name-spec-alist' has been been set, usually only one of
the arguments is specified.  If both are specified, then FORMAT-NAME
becomes a name for the format FILENAME specifies; if FORMAT-NAME is already
associated with a different format file, an error is signalled.

If the current format is unnamed, the user is prompted for a name
to give it, so that it can be conveniently restored if need be.  This
behavior is suppressed, and the record is not displayed, if the function is
not being called interactively.

The data display buffer is left in Database View mode.

Selecting the current format does not cause any work to be done.

Some databases automatically set the format of the record being displayed,
usually by setting `dbf-before-display-record-function' to a function that
overrides the format in effect when a record is about to be displayed.
This may cause this function to appear not to be doing any work.  In
actuality the format is being set, then reset."
  (interactive)

  (unless (and format-name
               (equal format-name (edb--S :format-name)))
    ;; We're not already in the requested format
    (db-view-mode)

    ;; If neither format- nor filename is specified, query for one of them.
    (unless (or format-name filename)
      (setq format-name
            (completing-read
             "Use which format? (? for options, RET to specify a file) "
             (cons '("") dbf-format-name-spec-alist)
             (lambda (elem)
               (stringp (car elem)))
             t))
      (when (equal "" format-name)
        (setq format-name nil
              filename (read-file-name "File for new format: " nil nil t))))

    ;; Either `format-name' or `filename' -- or possibly both,
    ;; if not called interactively -- is set.
    (when filename
      (setq filename (db-locate-format-file filename)))
    (when format-name
      (let ((file-name-handler-alist (cons
                                      (cons "^(connection)"
                                            'edb--connection-file-cache)
                                      file-name-handler-alist))
            (spec (cdr (assoc format-name dbf-format-name-spec-alist))))
        (if spec
            ;; successful format-name
            (let ((fs-filename (if (listp spec)
                                   (car spec)
                                 spec)))
              (if filename
                  (when (and fs-filename
                             ;; This test is required for interactive
                             ;; uses of `db-change-format'.
                             (not (db-same-file-p filename fs-filename)))
                    (error "Format name %s is associated with %s, not %s."
                           format-name fs-filename filename))
                (setq filename (db-locate-format-file fs-filename))))
          ;; unsuccessful format-name
          (if filename
              (push (cons format-name filename) dbf-format-name-spec-alist)
            ;; no filename, failed format-name
            (error "`%s' is not the name of a format." format-name)))))
    ;; Filename is now set.

    (flet ((mkspec (sumfmt sumfun)
                   ;; All of these items vary from format to format within a
                   ;; particular data display buffer.
                   (list
                    (edb--S :format-file)
                    ;; These can vary between data display buffers which
                    ;; happen to be using the same format file to specify the
                    ;; layout of the record's fields.  That is, these are
                    ;; specific to a particular data display buffer, not to a
                    ;; format, because they have to do with what is actually
                    ;; being displayed and/or because we might expect the user
                    ;; to change them after reading in the format.  This is
                    ;; why we can't just associate this information with the
                    ;; format file, but have to save it on a
                    ;; per-data-display-buffer basis.
                    sumfmt
                    sumfun
                    (edb--S :shown)
                    (edb--S :search-defaults))))

      ;; First save away current format.
      ;; No need to do anything with filename.
      (let ((curname (edb--S :format-name)))
        (when (and (interactive-p)
                   (not curname)
                   (y-or-n-p "Give the current format a name? "))
          (setq curname (read-string "Name for current format: "))
          (edb--S! :format-name curname))
        (when curname
          (let ((look (assoc curname dbf-format-name-spec-alist))
                (spec (mkspec (edb--S :sumfmt) (edb--S :sumfun))))
            (if look
                (setcdr look spec)
              (push (cons curname spec)
                    dbf-format-name-spec-alist)))))

      ;; Now install the new format.
      (let ((prev-format-file (edb--S :format-file))
            (fs (cdr (assoc filename (edb--S :fmtspec-stash)))))
        (edb--S! :format-name format-name)
        (edb--S! :format-file filename)
        (if fs
            (progn
              (mapc 'eval (edb--S! :always-forms (pop fs)))
              (edb--S! :displayspecs             (pop fs))
              (edb--S! :iftxt                    (pop fs))
              (edb--S! :fidx2dsidx               (car fs))
              (let ((spec (cdr (assoc (or (edb--S :format-name)
                                          (intern (edb--S :format-file)))
                                      dbf-format-name-spec-alist))))
                (edb--S! :format-file          (pop spec))
                (edb--S! :sumfmt               (pop spec))
                (edb--S! :sumfun               (pop spec))
                (edb--S! :shown                (pop spec))
                (edb--S! :search-defaults      (car spec))))
          ;; We didn't find `:format-file' in `:fmtspec-stash'; we probably
          ;; didn't find more than just a filename at `:format-name' in
          ;; dbf-format-name-spec-alist either.
          ;; This `let' is for the benefit of the new format file.
          (let ((file-name-handler-alist (cons
                                          (cons "^(connection)"
                                                'edb--connection-file-cache)
                                          file-name-handler-alist))
                (database dbc-database)
                (buffer-read-only nil))
            ;; Though we shamefully cover up the fact that the original format
            ;; file is being re-read (this time for caching purposes), this at
            ;; least allows us to no longer suggest setting the format name in
            ;; the format file.  All part of EDB 1.x janitorial services...
            ;; Also, don't mention anything for connection-bundled formats.
            (unless (or (memq (aref (edb--S :format-file) 0) '(?( ?)))
                        (equal prev-format-file (edb--S :format-file)))
              (db-message "Reading format from file: %s"
                          (edb--S :format-file)))
            (buffer-disable-undo)
            (erase-buffer)
            (insert-file-contents (edb--S :format-file))

            (db-really-hack-local-variables)

            (db-setup-ddb-parse-displayspecs dbc-database)

            ;; Save away the file-invariant stuff.
            (edb--S! :fmtspec-stash
                     (cons (list (edb--S :format-file)
                                 (edb--S :always-forms)
                                 (edb--S :displayspecs)
                                 (edb--S :iftxt)
                                 (edb--S :fidx2dsidx))
                           (edb--S :fmtspec-stash)))
            ;; Install the defaults under a symbol associated with the format
            ;; file (so it's not user-accessible).
            (push (cons (intern (edb--S :format-file))
                        (mkspec (edb--S :default-sumfmt)
                                (when (equal (edb--S :sumfmt)
                                             (edb--S :default-sumfmt))
                                  (edb--S :sumfun))))

                  dbf-format-name-spec-alist)
            (erase-buffer))))

      (when (interactive-p)
        (db-display-record (dbf-displayed-record) t)))))


(defun db-emergency-restore-format (&optional recompute)
  "Throw away the contents of the format buffer; redisplay the current record.
Use this if the format gets munged.
Changes made to the current field since last moving onto it may be lost.
If optional argument RECOMPUTE is non-nil, the displayed text
is recomputed as well."
  (db-display-record (dbf-displayed-record) recompute)
  (let ((this-fidx (edb--S :this-fidx)))
    (when this-fidx
      (dbf-set-this-field-modified-p nil)
      (db-move-to-field-exact this-fidx)
      ;; If the hook changed formats, we'll be in Database View mode.
      (db-edit-mode))))

(defun dbf-set-summary-format (summary-format)
  "Specify the format used in the Database Summary buffer.
Argument SUMMARY-FORMAT is a string containing display specifications.
Call this in the data display buffer, or in a format file or auxiliary file."
  (interactive (list (let ((sumfmt (edb--S :sumfmt)))
                       (read-string "Summary format: "
                                    (cons sumfmt 0) nil sumfmt))))
  (unless (stringp summary-format)
    (error "Argument to dbf-set-summary-format should be a string, not %s"
           summary-format))
  (when (= ?\n (elt summary-format (1- (length summary-format))))
    (setq summary-format (substring summary-format 0 -1)))
  (edb--S! :sumfmt summary-format)
  (dbf-set-summary-out-of-date-p)
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (let ((ht (edb--S :summaries)))
        (when ht (clrhash ht)))))
  (let ((lasfl (db-format->lines/sforms summary-format dbc-database 2 t nil)))
    ;; The (constant) number of screen lines occupied by each record summary,
    ;; computed automatically from the summary format.  This doesn't depend
    ;; on the field values in the individual records because
    ;; db-format->lines/sforms errs if min-height is not equal to max-height
    ;; (unless variable-height is set).  That makes determining which summary
    ;; point is in, and getting to a particular summary, much easier.
    (edb--1D! dbc-database :sum1lines (car lasfl))
    (edb--S! :sumfun `(lambda (formatted-record)
                        (concat ,@(cdr lasfl))))))


(defmacro dbf-always (&rest body)
  "Execute forms in BODY, and arrange to execute them in the future
each time that this format replaces another."
  (declare (debug body))
  `(progn
     (edb--S! :always-forms (nconc (edb--S :always-forms) ,body))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display data in a format
;;;

(defvar db-display-record-redo-inter-field-text-function nil
  "Function taking two args called in two contexts during `db-display-record'.
The args are BUF and EXTENTS-LIST.  In the first call, done right before
erasing the buffer, EXTENTS-LIST is null.  In the second call, done after
text has been inserted in the buffer and point is at point-min, EXTENTS-LIST
is a list of pairs of buffer positions in BUF that define the \"inter-field
text\", that is the text specifically not representing the data, but instead
part of the display format.")

(defun db-display-record (record &optional recompute fieldno-limit)
  ;; Why take RECORD argument instead of using var `dbf-displayed-record'?
  ;; Joe Wells has used this feature, so don't remove it.
  "Display RECORD in the current buffer, which is a data display buffer.
If optional arg RECOMPUTE is non-nil, the display representations will be
computed first; RECOMPUTE is typically non-nil only the first time a record
is shown.  If optional third arg FIELDNO-LIMIT is non-nil, only
fieldnumbers strictly less than it will be displayed."
  (let* ((buffer-read-only nil)
         (is-displayed-record-p (eq record (dbf-displayed-record)))
         ;; If quitting occurs in the middle of this operation, EDB becomes
         ;; very confused.  WARNING: Inhibitting quitting is dangerous.
         (inhibit-quit t)
         displayspecs shown len iftxt ds rep ext-start ift-extents)
    (run-hook-with-args 'dbf-before-display-record-function record)
    ;; Do after in case `dbf-before-display-record-function' changes things.
    (setq displayspecs (edb--S :displayspecs)
          shown (edb--S :shown)
          len (length displayspecs)
          iftxt (edb--S :iftxt))
    ;; Allow dbf-before-display-record-function to do
    ;; dbf-set-this-record-modified-p if it wants to.
    (when is-displayed-record-p
      (setq record (dbf-displayed-record)))
    (buffer-disable-undo)
    (run-hook-with-args 'db-display-record-redo-inter-field-text-function
                        (current-buffer) nil)
    (erase-buffer)
    (dotimes (fidx len)
      (setq ds (aref displayspecs fidx))
      (setq ext-start (point))
      (insert (aref iftxt fidx))
      (push (cons ext-start (point)) ift-extents)
      (when recompute
        (aset shown fidx
              (if (and fieldno-limit (>= fidx fieldno-limit))
                  ;; fixme: handle min-height and min-bytes. --ttn
                  (make-string (or (edb--1ds-min-width ds) 0) 32)
                (setq rep (db-ds-printed ds record))
                (replace-regexp-in-string
                 "\n"
                 (concat "\n" (make-string (current-column) 32))
                 rep))))
      (insert (aref shown fidx)))
    (setq ext-start (point))
    (insert (aref iftxt len))
    (push (cons ext-start (point)) ift-extents)
    (dbf-set-this-field-modified-p nil)
    ;; This place is as good as any for leaving the cursor by default.
    (goto-char (point-min))
    (buffer-enable-undo (current-buffer))
    (run-hook-with-args 'db-display-record-redo-inter-field-text-function
                        (current-buffer)
                        (nreverse ift-extents))
    ;; If quitting occurred while this was happening, ignore it.
    (setq quit-flag nil)))

(defun db-ds-printed (ds record)
  (let* ((ridx (edb--1ds-record-index ds))
         (rep (db-callconvert
               (edb--1ds-actual->display ds)
               (aref record ridx)
               record
               ridx)))
    (let ((min-h (edb--1ds-min-height ds))
          (max-h (edb--1ds-max-height ds)))
      (when (or min-h max-h)
        (let ((rep-h (1+ (db-count-newlines rep))))
          (cond ((and min-h (< rep-h min-h))
                 ;; too short
                 (setq rep
                       (concat rep
                               (make-string (- min-h rep-h)
                                            ?\n))))
                ((and max-h (> rep-h max-h))
                 ;; too tall
                 (setq rep
                       (substring rep 0
                                  (db-find-char-from-end
                                   ?\n rep
                                   (- rep-h min-h)))))))))

    ;; These conditions are much too simplistic; they only work for one-line
    ;; representations.
    (let ((rep-w (length rep))
          (min-w (edb--1ds-min-width ds))
          (max-w (edb--1ds-max-width ds)))
      (cond ((and min-w (< rep-w min-w))
             ;; The display representation is too short
             (setq rep (funcall (or (edb--1ds-padding-action ds)
                                    'db-pad-right)
                                min-w rep rep-w))
             (unless (= (length rep) min-w)
               (error "Padding function %s returned \"%s\", %s %d, not %d."
                      (or (edb--1ds-padding-action ds)
                          'db-pad-right)
                      rep "which has length" (length rep) min-w))
             (setq rep-w min-w))
            ((and max-w (> rep-w max-w))
             ;; The display representation is too long.
             (funcall (or (edb--1ds-truncation-display-action ds)
                          (lambda (max-w rep rep-w)
                            (put-text-property max-w rep-w
                                               'invisible t
                                               rep)))
                      max-w rep rep-w)
             ;; Assume the truncation function did the right thing.
             (setq rep-w max-w)))
      rep)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc.
;;;

(defun database-mode ()
  "A mode for viewing and editing formatted data; a database front end.
In Database Edit mode, fields of the database may be changed.
In Database View mode, keystrokes are bound to database commands.
Typically, if point is on a field, the buffer is in Database Edit mode; if
point is at the beginning of the buffer, the buffer is in Database View mode.
The mode line indicates which mode the buffer is in.

Database View mode key bindings:

\\{database-view-mode-map}

Database Edit mode key bindings:

\\{database-edit-mode-map}"

  (setq mode-line-modified '(:eval (let ((meta (edb--meta1D dbc-database)))
                                     (if (not (gethash :modifiable-p meta))
                                         "%%%%"
                                       (string
                                        (if (gethash :modp meta) ?* ?-)
                                        (if (edb--S :utkmodp) ?* ?-)
                                        ))))
        mode-line-format '("-"
                           mode-line-modified
                           "%*"
                           "- %17b   %[("
                           mode-name
                           minor-mode-alist
                           (:eval (concat (and (edb--S :hide-p) " Hide")
                                          " "
                                          (edb--S :index-fraction)))
                           ")%]"
                           "---"
                           (-3 . "%p")
                           "-%-"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (db-view-mode))

(defsubst db-data-display-buffer-p ()
  "T if this buffer is a database data display buffer."
  (memq major-mode '(database-view-mode database-edit-mode)))

;;; db-format.el ends here
;;; db-file-io.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Read and write database files.

;;; Code:


(require 'pp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB variables
;;;

(defvar db-format-file-path nil
  "List of directories (strings) to search, in order, for format files not
found in the directory with their associated databases.")

(defvar db-aux-file-path nil
  "List of directories (strings) to search, in order, for auxiliary files not
found in the directory with their associated databases.")

(put 'db-before-read-hooks 'safe-local-variable 'edb-hookp)
(defvar db-before-read-hooks nil
  "Normal hook run immediately before a database is first read
but after all local variables are set.
The hooks are run in the data display buffer with variable `database' bound.
Variable `db-buffer' is bound to a buffer containing the database file.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file\), then consider
having its last action be to reset the variable to nil.")

(put 'db-after-read-hooks 'safe-local-variable 'edb-hookp)
(defvar db-after-read-hooks nil
  "Function or list of functions run after a database is completely read.
The hooks are run in the data display buffer with variable `database' bound.
For databases with nonregular layouts, you might put a call to
`database-stored->actual' here, for instance.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file), then consider
having its last action be to reset the variable to nil.")

(defvar db-format-file-suffixes '(".dbf" ".fmt" "f")
  "List of format file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dbf\" \".fmt\" \"f\").
The . that may precede the extension must be specified explicitly.")

(defvar db-aux-file-suffixes '(".dba" ".aux" "a")
  "List of auxiliary file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dba\" \".aux\" \"a\").
The . that may precede the extension must be specified explicitly.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables
;;;

;; These have docstrings so docstring-substitute will recognize them.

(eval-when-compile
  (defvar db-buffer nil "Buffer containing a database file being read."))

(eval-when-compile
  (defvar database nil "Database being read from a file; also other uses."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a database file
;;;

(defun db-read-database-file (source &optional display-template confirm)
  ;; Return the database.  The data display buffer will be the first element
  ;; of the :ddbufs list in the global hash.
  ;;
  ;; SOURCE is either a filename (string), or a list: (FILENAME RECORD...),
  ;; the latter indicating inherent data.
  ;;
  ;; Optional arg DISPLAY-TEMPLATE is one of the following:
  ;; - buffer containing the format to use;
  ;; - filename that specifies the format file;
  ;; - nil, in which case the one computed from `db-format-file-path' and
  ;;   `db-format-file-suffixes' will be used, or the user will be prompted
  ;;   for one.
  ;;
  ;; If optional prefix arg CONFIRM is non-nil, then no default will be used
  ;; without confirmation from the user.
  (edb--G! :io-error-p nil)
  (with-temp-buffer
    (let ((buf (current-buffer))
          (known-records (when (consp source)
                           (cdr source)))
          (db-file (when (stringp source)
                     source))
          (format-file (when (stringp display-template)
                         display-template))
          ddb database coding)

      ;; sanity check: mutual exclusivity
      (assert (= 1 (logxor (if known-records 1 0) (if db-file 1 0))))

      (when db-file
        (if (file-exists-p db-file)
            (insert-file-contents db-file nil)
          (message "New database file.")))

      (setf database (or (db-read-database-internal-file-layout)
                         (edb--make-v1-monolithic-mess)))
      (edb--meta1D database)
      (edb--1D! database :file (if known-records
                                   (propertize "(inherent)" :file (car source))
                                 db-file))
      (edb--1D! database :modifiable-p (if known-records
                                           t ; hack
                                         (file-writable-p db-file)))

      (when (and (not (bufferp display-template))
                 (or (not format-file) confirm))
        (setq format-file (db-choose-format-file database format-file
                                                 confirm)))

      (setq ddb (db-setup-data-display-buffer
                 (or format-file display-template)
                 database t))
      (let ((ddbufs (edb--1D database :ddbufs)))
        (edb--1D! database :ddbufs (cons ddb ddbufs)))

      (with-current-buffer ddb
        ;; reread data if coding system is different
        (when (and (boundp 'edb-data-coding)
                   (setq coding (symbol-value 'edb-data-coding)))
          (db-message "edb-data-coding: %s" coding)
          (setq buffer-file-coding-system coding)
          (with-current-buffer buf
            (unless (eq buffer-file-coding-system coding)
              (erase-buffer)
              (let ((coding-system-for-read coding))
                (insert-file-contents db-file nil))
              ;; hack! -- fixme: separate internal layout check/read, to
              ;;                 avoid ugliness that is the undo/redo.  --ttn
              (when (edb--1D database :togp)
                (kill-buffer (car (edb--1D database :ddbufs)))
                (setf database (db-read-database-internal-file-layout))
                (edb--1D! database :file db-file)
                (edb--1D! database :modifiable-p (file-writable-p db-file))
                (setq ddb (db-setup-data-display-buffer
                           (or format-file display-template)
                           database t))
                (let ((ddbufs (edb--1D database :ddbufs)))
                  (edb--1D! database :ddbufs (cons ddb ddbufs)))
                (with-current-buffer ddb
                  (setq buffer-file-coding-system coding)))))))

      (with-current-buffer ddb
        (let ((db-buffer buf))
          (run-hooks 'db-before-read-hooks)))

      (if known-records
          (edb--snap! database (length known-records) known-records)
        (db-read-database-file-helper database))

      (with-current-buffer ddb
        (run-hooks 'db-after-read-hooks))

      database)))


(defun db-read-database-internal-file-layout ()
  ;; If the buffer contains a database in internal file layout, read and
  ;; return the header portion (not the records).  Otherwise, return nil.
  (when (db-skip-string-forward ";; Database file written by EDB")
    (let ((here (point))
          (emacs-lisp-mode-hook nil)
          cruft)
      (emacs-lisp-mode)
      ;; Update old formats in small increments.
      (when (db-skip-string-forward "; format 0.1")
        (delete-char -1)
        (insert "2")
        (forward-sexp)
        (backward-char 1)
        ;; add locals slot to database
        (insert " nil")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.2")
        (delete-char -1)
        (insert "3")
        (forward-sexp)
        (backward-char 1)
        (backward-sexp 1)
        (backward-char 1)
        ;; add modified bit to database
        (insert " nil")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.3")
        (delete-char -1)
        (insert "4")
        (down-list 1)
        (forward-sexp 16)
        ;; add "internal file layout" slot to database
        (insert " t")
        (forward-sexp 14)
        ;; add modifiable bit to database
        (insert " t")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.4")
        (delete-char -1)
        (insert "5")
        ;; change "[database" to "[cl-struct-database"
        (down-list 1)
        (insert "cl-struct-")
        (forward-sexp 17)
        ;; change "[sepinfo" to "[cl-struct-sepinfo"
        (down-list 1)
        (insert "cl-struct-")
        (up-list 1)
        (down-list 1)
        (insert "cl-struct-")
        (up-list 1)
        (down-list 1)
        (insert "cl-struct-")
        (goto-char here))
      (flet ((skip-zonk (s z) (let ((p (progn (forward-sexp s) (point))))
                                (forward-sexp z)
                                (delete-region p (point)))))
        (when (db-skip-string-forward "; format 0.5")
          (delete-char -1)
          (insert "6")
          ;; Remove five slots for quotation information.
          (down-list 1)
          (skip-zonk 24 5)
          (goto-char here))
        (when (db-skip-string-forward "; format 0.6")
          (delete-char -1)
          (insert "7")
          (down-list 1)
          ;; Remove structure tag.
          (skip-zonk 0 1)
          ;; Remove slots for data container, number of records, filename,
          ;; "file-local" variables, aux filename, buffer list, default format
          ;; filename (but keep as cruft), hide functions, number of fields.
          (skip-zonk 1 6)
          (let ((filename (read (current-buffer))))
            (when filename (push (cons :format-file filename) cruft)))
          (skip-zonk -1 3)
          ;; Remove slot for field name to number alist.
          (skip-zonk 1 1)
          ;; Remove slots for "hidden to end" and "internal file layout" bits.
          (skip-zonk 2 2)
          ;; Remove structure tags.
          (down-list 1)
          (skip-zonk 0 1)
          (backward-up-list 1)
          (forward-sexp 1)
          (down-list 1)
          (skip-zonk 0 1)
          (backward-up-list 1)
          ;; Remove slot for alternatives sepinfo.
          (skip-zonk 1 1)
          ;; Remove slots for mod bits.
          (skip-zonk 5 2)
          ;; Delete parens around record data.
          (re-search-forward "^($")
          (delete-region (1- (match-beginning 0)) (match-end 0))
          (save-excursion
            (re-search-forward "^).*$")
            (delete-region (1- (match-beginning 0)) (match-end 0)))
          ;; Add cruft.
          (insert (format "\n%S\n" cruft))
          (goto-char here)))

      ;; Don't forget to change `db-write-1' when updating the format number.
      (unless (db-skip-string-forward "; format 0.7")
        (db-message "I don't know if I can read the database, but I'll try"))
      (end-of-line)
      (let ((rv (read (current-buffer)))
            (cruft (read (current-buffer))))
        (edb--meta1D rv)
        (edb--1D! rv :togp t)
        (when cruft (edb--1D! rv :1CRUFT cruft))
        ;; Deconstruct so that `database-set-fieldnames-to-list'
        ;; can elaborate (after reconstructing).  Ugly shit!
                  (let* ((names (database-fieldnames rv))
                         (count (length names))
               (types (database-recordfieldspecs rv))
               name type)
          (setf (database-fieldnames rv) nil
                (database-recordfieldspecs rv) nil)
          (database-set-fieldnames-to-list
           rv (mapcar (lambda (fidx)
                        (if (setq name (aref names fidx)
                                  type (aref types fidx))
                            (cons name type)
                          name))
                      (number-sequence 0 (1- count)))))
        ;; TODO: Factor this and same in `db-setup-data-display-buffer'.
        (unless (database-field-priorities rv)
          (setf (database-field-priorities rv)
                (list (mapcar 'list (number-sequence
                                     0 (1- (length (database-fieldnames
                                                    rv))))))))
        rv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format file
;;;

(defun db-locate-format-file (filename)
  ;; Return a full pathname for the format file named FILENAME, or err.
  (or (db-locate-readable-file-prefer-cwd
       (file-name-nondirectory filename)
       db-format-file-path)
      (error "I can't find a format file named %s." filename)))

(defun db-choose-format-file (db ff-default confirm)
  ;; Return a format file according to DB or FF-DEFAULT.
  ;; Prompt if CONFIRM is set or if we can't get one from DB alone.
  (let* ((db-file (edb--1D db :file))
         (default (or ff-default
                      (cdr (assq :format-file (edb--1D db :1CRUFT)))
                      (db-locate-readable-file-prefer-cwd
                       (file-name-nondirectory
                        (file-name-sans-extension db-file))
                       (cons (file-name-directory db-file)
                             db-format-file-path)
                       db-format-file-suffixes))))
    (if (and default (not confirm))
        (let* ((dir (file-name-directory db-file))
               (default-directory (if dir
                                      (expand-file-name dir)
                                    default-directory)))
          (db-locate-format-file default))
      ;; Don't need `db-locate-format-file' because MUSTMATCH arg
      ;; to `read-file-name' is t.
      (expand-file-name
       (read-file-name
        (if default
            (format "Display format for %s: [default %s] "
                    (file-name-nondirectory db-file)
                    (file-name-nondirectory default))
          (format "Display format for %s: "
                  (file-name-nondirectory db-file)))
        (file-name-directory db-file)
        (when default
          (expand-file-name default (file-name-directory db-file)))
        t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database file:  helper functions
;;;

(defun db-read-database-file-helper (db)
  ;; Read current buffer into database DB and return DB.
  (db-message "Reading database...")
  ;; The format and the auxiliary file should already be loaded; ie, all
  ;; special variables should be set.
  (if (edb--1D db :togp)
      (let* ((new (list nil))
             (tp new)
             (count 0))
        (while (progn (skip-chars-forward " \n")
                      (< (point) (point-max)))
          (setcdr tp (list (read (current-buffer))))
          (setq count (1+ count)
                tp (cdr tp)))
        (edb--snap! db count (cdr new)))

    (database-io-setup db t)

    ;; Remove pre-first-record- and post-last-record strings or regexps, then
    ;; add a record separator to the end so that every record is terminated by
    ;; one.  Try the {pre-first,post-last}-regexp first.  On match, save the
    ;; text in the associated {pre-first,post-last}-string for output later.
    (let ((rsep (database-record-sepinfo db))
          rgx str p)

      ;; Remove gubbish before first record.
      (goto-char (point-min))
      (when (setq p (cond ((setq rgx (sepinfo-pre-first-regexp rsep))
                           (unless (db-skip-regexp-forward rgx)
                             (error "Unmatched pre-first regexp"))
                           (match-end (sepinfo-pre-first-regexp-submatch rsep)))
                          ((setq str (sepinfo-pre-first-string rsep))
                           (unless (db-skip-string-forward str)
                             (error "Missing pre-first string"))
                           (point))
                          (t nil)))
        (if rgx
            (setf (sepinfo-pre-first-string rsep)
                  (delete-and-extract-region (point-min) p))
          (delete-region (point-min) p)))

      ;; Remove gubbish after last record.
      (goto-char (point-max))
      (when (setq p (cond ((setq rgx (sepinfo-post-last-regexp rsep))
                           (unless (re-search-backward rgx)
                             (error "Unmatched post-last regexp"))
                           (match-beginning
                            (sepinfo-post-last-regexp-submatch rsep)))
                          ((setq str (sepinfo-post-last-string rsep))
                           (unless (search-backward str)
                             (error "Missing post-last string"))
                           (point))
                          (t nil)))
        (if rgx
            (setf (sepinfo-post-last-string rsep)
                  (delete-and-extract-region p (point-max)))
          (delete-region p (point-max))))

      ;; If there isn't one there already, add a final record separator so
      ;; that every record is terminated by one.
      (let ((sep (sepinfo-sep-string rsep)))
        (unless (db-skip-string-backward sep)
          (insert sep))))

    (if (database-read-record-from-region db)
        (db-read-file-custom db)
      (db-read-file-delimited db)))
  (edb--meta1D db)
  (db-message "Reading database...done (%d records)" (edb--1D db :nrecords))
  db)


(defun edb--snap! (db len ls)
  ;; Take a list of records and set DB's `:vov' and `:nrecords'.
  (let ((vov (make-vector (+ len 10) nil)))
    (dotimes (i len)
      (aset vov i (pop ls)))
    (edb--1D! db :vov vov)
    (edb--1D! db :nrecords len)))


(defmacro db-reading-noninternal (condition &rest body)
  (declare (indent 1) (debug (form body)))
  `(let* ((new (list nil))
          (tp new)
          (count 0)                     ; binding used in BODY
          this-record)                  ; value set by BODY
     (while ,condition
       (setq count (1+ count))
       ;; Noisy, obscures warning messages.
       (if (and db-inform-interval
                (zerop (% count db-inform-interval)))
           (message "Reading database...%d" count))
       ,@body                           ; uses binding `database'
       (setcdr tp (list this-record))
       (setq tp (cdr tp)))
     (edb--snap! db count (cdr new))))


;; fixme: integrate and refactor db-read-file-* funcs. --ttn

(defun db-read-file-custom (db)
  (let ((rrfunc (database-read-record-from-region db))
        (nfields (length (database-fieldnames db)))
        (nm2no (edb--1D db :nm2no))
        (rsi (database-record-sepinfo db))
        ;; documented dynamic binding
        (database db))
    ;; Uses `database-read-record-from-region' and `sepinfo-sepfunc' from
    ;; `database-record-sepinfo'.
    ;; Check that the sepinfo has enough information.
    (if (sepinfo-sep-regexp rsi)
        (unless (sepinfo-sep-regexp-submatch rsi)
          (error "Missing submatch for sep-regexp `%s' of record sepinfo."
                 (sepinfo-sep-regexp rsi)))
      (unless (sepinfo-sep-function rsi)
        (let ((sep (sepinfo-sep-string rsi)))
          (when (or (not sep)
                    (equal "" sep))
            (error "Missing specification in record sepinfo."))
          (db-jam-si sep rsi))))

    (if (sepinfo-sep-function rsi)
        ;; Use sep-function to delimit the records

        ;; Return a pair of (end-pos . start-pos).
        (let ((delim (sepinfo-sep-function rsi))
              ;; Read-record-from-region might do insertions or deletions,
              ;; so just remembering the position isn't enough.
              next-start-marker
              (prev-end-pos 'nil)
              (end-start-pair '(nil)))
          (goto-char (point-min))
          (setq next-start-marker (point-marker))

          ;; Assume that there is at least one record in the file.
          ;; fixme: validate first. --ttn
          (db-reading-noninternal (marker-position next-start-marker)

            (setq end-start-pair (funcall delim prev-end-pos))
            (narrow-to-region (marker-position next-start-marker)
                              (car end-start-pair))
            ;; Writers of record->region often forget to do this.
            (goto-char (point-min))
            (setq this-record (let ((v (funcall rrfunc)))
                                (if (vectorp v)
                                    v
                                  (db--mkrec nfields nm2no v))))
            ;; Not `(car end-start-pair)' in case buffer was modified.
            (setq prev-end-pos (point-max))
            (widen)
            ;; In 18.55, `set-marker' can only make markers point to the
            ;; accessible portion of the buffer, so do this after widening.
            ;; (This may have changed by 18.59.)
            ;; Don't use just `(cdr end-start-pair)' because the buffer may
            ;; have been modified.
            (set-marker next-start-marker (and (cdr end-start-pair)
                                               (+ (- (cdr end-start-pair)
                                                     (car end-start-pair))
                                                  prev-end-pos)))))
      ;; Use regexp to delimit the records.
      (let ((rsep-rgx (sepinfo-sep-regexp rsi))
            (rsep-rgx-subm (sepinfo-sep-regexp-submatch
                            rsi))
            (next-start (point-min))
            ;; How many characters to skip before looking for the start of the
            ;; next record.  We can't use a position because rrfunc may insert
            ;; or delete, and we can't use markers because `narrow-to-region'
            ;; squeezes them.
            sep-len)
        ;; The caller also moved point.
        (goto-char (point-min))
        (db-reading-noninternal (< next-start (point-max))
          (goto-char next-start)
          ;; re-search-forward errs if it fails
          (re-search-forward rsep-rgx)
          (setq sep-len (- (match-end rsep-rgx-subm)
                           (match-beginning rsep-rgx-subm)))
          (narrow-to-region next-start (match-beginning rsep-rgx-subm))
          (goto-char (point-min))
          (setq this-record (let ((v (funcall rrfunc)))
                                (if (vectorp v)
                                    v
                                  (db--mkrec nfields nm2no v))))
          (setq next-start (+ (point-max) sep-len))
          (widen))))))



(defun db-read-file-delimited (db)
  ;; If there are any regexps set in the sepinfos at this point, then we
  ;; assume that the programmer specified them explicitly; we assume that if
  ;; any substitutions are requested, then the programmer knows that they
  ;; won't cause any ambiguities.

  (goto-char (point-min))

  ;; We have now cleared away the pre- and post- garbage.
  (if (or (sepinfo-sep-regexp (database-field-sepinfo db))
          (sepinfo-sep-regexp (database-record-sepinfo db)))
      ;; Regexps involved, so do no substitution on the separators, except for
      ;; those that the user has explicitly requested; then read the database.
      (progn
        (database-perform-substitutions db t)
        (db-read-file-delimrx db))

    ;; There are no regexps involved.
    (let* ((fsep-str (database-full-fieldsep-string db))
           (rsep-str (database-full-recordsep-string db))
           conftup)

      ;; Convert the database buffer from "complex" to "simple" form.
      (let* ((fsi (database-field-sepinfo db))
             (pre-str (sepinfo-pre-first-string fsi))
             (pre-rgx (sepinfo-pre-first-regexp fsi)))
        ;; Previously, we only did the record separators;
        ;; here we do the field separators as well.
        (goto-char (point-min))
        (cond (pre-rgx
               (if (db-skip-regexp-forward pre-rgx)
                   (delete-region (point-min) (point))
                 (error "Missing regexp `%s' before first field."
                        pre-rgx)))
              (pre-str
               (if (db-skip-string-forward pre-str)
                   (delete-region (point-min) (point))
                 (error "Didn't find string `%s' leading the first field."
                        pre-str))))

        (goto-char (point-max))
        ;; Don't get rid of post-last-field-regexp or post-last-field-string
        ;; because we look for them at the end of every record.

        ;; Make sure that record-sepinfo-sep-string appears at the end.
        (when (sepinfo-sep-string (database-record-sepinfo db))
          (if (db-skip-string-backward
               (sepinfo-sep-string (database-record-sepinfo db)))
              (goto-char (point-max))
            (insert (sepinfo-sep-string (database-record-sepinfo db)))))
        (when pre-str
          (insert pre-str)))

      ;; We're still inside the big let.

      ;; This pre-read confirmation should be optional.  And it should be
      ;; able to deal with regexps.
      ;; When would this test fail?  Won't fsep-str and
      ;; rsep-str always be non-nil when we get here?
      (when (and fsep-str rsep-str)
        (db-message "Confirming database format...")
        (setq conftup (db-confirm-seps fsep-str rsep-str
                                       (length (database-fieldnames db))
                                       nil))
        (if (or (db-conftup-bad-fsep conftup)
                (db-conftup-bad-rsep conftup))
            (progn
              (db-warning "The database file is malformed!")
              (when (db-conftup-bad-fsep conftup)
                (db-warning "Extra field separator %s found in data."
                            (pp-to-string fsep-str)))
              (when (db-conftup-bad-rsep conftup)
                (db-warning "Extra record separator %s found in data."
                            (pp-to-string rsep-str)))
              ;; show the db warning buffer
              (if (yes-or-no-p "Bad file format; try reading anyway? ")
                  (db-message "Damaged file; expecting circa %d records"
                              (db-conftup-reccount conftup))
                (kill-buffer nil)
                (error "Aborted attempt to read database.")))
          (db-message "Database looks OK from here; expecting %d records"
                      (db-conftup-reccount conftup))))

      ;; mernst sez: This also sets the io-sep variables.
      ;; ttn sez: ???

      ;; Convert field/record separators (perhaps choosing new ones), so
      ;; they won't get damaged by the substitution, and then doing the
      ;; substitution.  It also must set the sub-{field,record}sep slots,
      ;; because later on those fields are slavishly followed.  We can't
      ;; parse or do substitutions if either of the separators is a regexp.
      (if (or (sepinfo-sep-regexp (database-record-sepinfo db))
              (sepinfo-sep-regexp (database-field-sepinfo db)))
          (setf (database-sub-recordsep-string db)
                (or (sepinfo-sep-regexp (database-record-sepinfo db))
                    (database-sub-recordsep-string db)
                    rsep-str)
                (database-sub-fieldsep-string db)
                (or (sepinfo-sep-regexp (database-field-sepinfo db))
                    (database-sub-fieldsep-string db)
                    fsep-str))
        (unless rsep-str
          (error "No record separator specified."))
        (if (database-acceptable-delimiter-p db rsep-str)
            (setf (database-sub-recordsep-string db) rsep-str)
          (db-message "Substituting record delimiter for read...")
          (setf (database-sub-recordsep-string db)
                (database-generate-delimiter db))
          (goto-char (point-min))
          (let ((s (database-sub-recordsep-string db)))
            (while (search-forward rsep-str nil t)
              (replace-match s nil t)))
          (setq fsep-str (replace-regexp-in-string
                          rsep-str (database-sub-recordsep-string db)
                          fsep-str))
          (db-message "Substituting record delimiter for read...done"))
        (if (or (database-acceptable-delimiter-p db fsep-str)
                (database-read-record-from-region db))
            (progn
              (db-message "fsep-str `%s' acceptable because `(or %s %s)'"
                          (string-to-list fsep-str)
                          (database-acceptable-delimiter-p db fsep-str)
                          (database-read-record-from-region db))
              (setf (database-sub-fieldsep-string db) fsep-str))
          (unless fsep-str
            (error "No field separator specified."))
          (db-message "Substituting field delimiter for read...")
          (setf (database-sub-fieldsep-string db)
                (database-generate-delimiter db))
          (db-message "Substituting field delimiter... (`%s' for `%s')"
                      (string-to-list (database-sub-fieldsep-string db))
                      (string-to-list fsep-str))
          (goto-char (point-min))
          (let ((s (database-sub-fieldsep-string db)))
            (while (search-forward fsep-str nil t)
              (replace-match s nil t)))
          (db-message "Substituting field delimiter for read...done"))
        (db-message "sub-fieldsep: %s"
                    (string-to-list (database-sub-fieldsep-string db)))

        (database-perform-substitutions db t))

      (db-read-file-delimstr db))))


(defun db-read-file-delimstr (db)
  ;; When we call this, the database is in the following form:
  ;; Point at start of first field of first record.
  ;; Each field, except last, is ended by actual-fieldsep.
  ;; Each record, including last, is ended by actual-recordsep.
  ;; End of last recordsep-string = eob.
  ;; Field-sep and record-sep must be strings.
  (db-message "Reading database...")

  (goto-char (point-min))
  (let* ((fsep (database-sub-fieldsep-string db))
         (rsep (database-sub-recordsep-string db))
         (flen (length fsep))
         (rlen (length rsep))
         (nfields (length (database-fieldnames db)))
         (max-fno (1- nfields))
         (default-slice (db-rs-slice db 'edb--1rs-default-value))
         (here (point))
         fno
         end-of-rsep
         end-of-record)

    (db-reading-noninternal (not (or (eobp)
                                     ;; Does this cause any problems?
                                     ;; Special case for rsep = "\n\n",
                                     ;; extra newline at end
                                     (and (string= rsep "\n\n")
                                          (looking-at "\n\\'"))))
      (setq this-record (make-vector nfields nil))
      (if (search-forward rsep nil t)
          (setq end-of-rsep (point)
                end-of-record (- (point) rlen))
        (db-warning "Didn't find %s at end of last field of record %d, %s!"
                    (pp-to-string rsep) count "and I put it there myself")
        (setq end-of-rsep (point-max)
              end-of-record (point-max)))
      (goto-char here)

      (setq fno 0)
      (while (< fno max-fno)
        ;; fixme: trap errors, maybe check for rsep in field data.  --ttn
        (if (search-forward fsep end-of-record t)
            (progn
              (aset this-record fno (buffer-substring here (- (point) flen)))
              (setq here (point))
              (incf fno))

          (db-warning "%s %d fields of record %d (didn't find fsep %s)"
                      "Hit the end of the record after"
                      fno count (pp-to-string fsep))
          (aset this-record fno (buffer-substring here end-of-record))
          (setq here end-of-record)
          (incf fno)
          (while (<= fno max-fno)
            (aset this-record fno (aref default-slice fno))
            (incf fno))))
      ;; Weren't too few fields, so set the last one (else it's already set).
      (when (= fno max-fno)
        (when (search-forward fsep end-of-record t)
          (db-warning "Extra fields in record %d %s."
                      count "packed into the last field; beware when writing")
          (edb--G! :io-error-p t))
        (aset this-record max-fno (buffer-substring here end-of-record)))
      (goto-char end-of-rsep)
      (setq here (point)))

    ;; Convert from stored to actual format.
    (database-stored->actual db)

    ;; Function is called for side-effect, but return the database anyway.
    db))


(defun db-read-file-delimrx (db)

  (db-message "Reading database...")

  (goto-char (point-min))

  (let* ((fsi (database-field-sepinfo db))
         (rsi (database-record-sepinfo db))
         (frx (or (sepinfo-sep-regexp fsi)
                  (regexp-quote (sepinfo-sep-string
                                 (database-field-sepinfo db)))))
         (frx-subm (or (sepinfo-sep-regexp-submatch fsi)
                       0))
         (rrx (or (sepinfo-sep-regexp rsi)
                  (regexp-quote (sepinfo-sep-string rsi))))
         (rrx-subm (or (sepinfo-sep-regexp-submatch rsi)
                       0))

         ;; Do not fold into `rrx'; they may have submatches of their own.
         (pre-str (sepinfo-pre-first-string fsi))
         (pre-frx (or (sepinfo-pre-first-regexp fsi)
                      (and pre-str
                           (regexp-quote pre-str))))
         (pre-frx-subm (or (sepinfo-pre-first-regexp-submatch
                            fsi)
                           0))
         (post-str (sepinfo-post-last-string fsi))
         (post-frx (or (sepinfo-post-last-regexp fsi)
                       (and post-str
                            (regexp-quote post-str))))
         (post-frx-subm (or (sepinfo-post-last-regexp-submatch
                             fsi)
                            0))

         (nfields (length (database-fieldnames db)))
         (max-fno (1- nfields))
         (here (point))
         fno
         end-of-rrx
         end-of-record)

    (db-reading-noninternal (not (eobp))
      (setq this-record (make-vector nfields nil))
      (when pre-frx
        (if (db-skip-regexp-forward pre-frx)
            (progn
              (setq here (match-end pre-frx-subm))
              (goto-char here))
          (error "Didn't find pre-first stuff I expected.")))

      (setq fno 0)
      (while (< fno max-fno)
        ;; fixme: trap errors, maybe check for rrx in field data.  --ttn
        (if (re-search-forward frx nil t)
            (progn
              (aset this-record fno
                    (buffer-substring here (match-beginning frx-subm)))
              (setq here (match-end frx-subm))
              (incf fno))
          (db-warning "%s %d fields of record %d (didn't find frx `%s')"
                      "End of data after"
                      fno count frx)
          (setq fno max-fno)))
      (if (re-search-forward rrx nil t)
          (setq end-of-rrx (match-end rrx-subm)
                end-of-record (match-beginning rrx-subm))
        (db-warning "Didn't find %s at end of last field of record %d."
                    (pp-to-string rrx) count)
        (setq end-of-rrx (point-max)
              end-of-record (point-max)))
      (goto-char here)
      (when (re-search-forward frx end-of-record t)
        (db-warning "Too many fields in record %d; %s; beware when writing."
                    count "packing them all into the last field")
        (edb--G! :io-error-p t))
      (aset this-record max-fno (buffer-substring here end-of-record))
      (goto-char end-of-rrx)
      (setq here (point)))

    (database-stored->actual db)

    db))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database utilities
;;;


(defun database-stored->actual (&optional db)
  ;; Convert string values in a newly-read database to the actual format.  If
  ;; DB is not specified, use the value of the dynamic variable `database'.
  ;; This makes it possible to be put directly in `db-after-read-hook'.
  (unless db (setq db database))
  (let* ((no-of-fields (length (database-fieldnames db)))
         (convert (make-vector no-of-fields nil))
         (s->a-slice (db-rs-slice db 'edb--1rs-stored->actual))
         s->a todo val)
    (dotimes (fno no-of-fields)
      (when (setq s->a (aref s->a-slice fno))
        (push fno todo)
        (aset convert fno s->a)))
    (when todo
      (db-message "Converting from stored record format...")
      (db-maprecords (lambda (record)
                       (dolist (fno todo)
                         (setq val (aref record fno))
                         (aset record fno (if (stringp val)
                                              (funcall (aref convert fno) val)
                                            val))))
                     db nil "Converting record format...%s")
      (db-message "Converting record format...done"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write database file
;;;


(defun db-write-1 ()
  (edb--G! :io-error-p nil)
  ;; This is called by `db-write-database-file' with the data display buffer
  ;; current, and we need the values of variables local to that buffer.
  (let ((orig-buf (current-buffer))
        (ofile (edb--1D dbc-database :file))
        (inherent (edb--1D dbc-database :inherent))
        (db dbc-database)
        (coding buffer-file-coding-system))
    (when (file-directory-p ofile)
      (edb--G! :io-error-p t)
      (error "Invalid filename: %s" ofile))
    (unless (file-writable-p ofile)
      (if (and (not noninteractive)
               (y-or-n-p (concat "File not writable: " ofile
                                 "\nMake it writable and continue? ")))
          (set-file-modes ofile (logior #o200 (file-modes ofile)))
        (edb--G! :io-error-p t)
        (error "File not writable: %s" ofile)))
    (with-temp-buffer
      (cond
       ;; Data inherent data to the connection.
       (inherent
        (flet ((iget (k) (gethash k inherent))
               (bget (k) (car (iget k)))
               (bput (k v) (setcar (iget k) v)))
          (let* ((all (db-maprecords 'identity db nil nil t))
                 (zonkp nil)
                 (cbuf (cond ((eq 'buffer (iget :control-type))
                              (get-buffer (iget :control-name)))
                             ((find-buffer-visiting (iget :control-fname)))
                             (t (setq zonkp t)
                                (find-file (iget :control-fname))))))
            (unless cbuf
              (error "Control no longer exists for %S"
                     (buffer-name orig-buf)))
            (with-current-buffer cbuf
              (save-excursion
                (funcall (edb--rget '(:edb1 :seq-funcs) (iget :seqw))
                         (bget :start-box)
                         (bget :finish-box)
                         all)
                (bput :finish-box (point)))
              (if (not (setq ofile (iget :control-fname)))
                  (db-message "Buffer %S updated (but not written to a file)"
                              (iget :control-name))
                (unless (file-writable-p ofile)
                  (error "File not writable: %s" ofile))
                (when (file-directory-p ofile)
                  (error "Invalid filename: %s" ofile))
                (write-file ofile)
                (when zonkp
                  (kill-buffer nil)))
              (database-set-modified-p db nil)))))
       ;; "Internal" representation -- sigh.
       ((edb--1D db :togp)
        (let ((standard-output (current-buffer)))
          (setq buffer-file-coding-system coding)
          (insert ";; Database file written by EDB; format 0.7")
          (let ((coding-string (symbol-name coding)))
            (unless (string-match "^undecided-" coding-string)
              (insert " -*- coding: " coding-string "; -*-")))
          (insert "\n")
          (let ((copy (edb--copy-v1-monolithic-mess db))
                (nfields (length (database-fieldnames db)))
                (prios (database-field-priorities db)))
            (setf (database-field-priorities copy)
                  (if (equal (list (mapcar 'list (number-sequence
                                                  0 (1- nfields))))
                             prios)
                      nil
                    prios))
            (pp copy))
          (let ((cruft (edb--1D db :1CRUFT)))   ; blech
            (if cruft                           ; even more blech
                (pp cruft)
              (insert "nil\n")))
          (db-maprecords 'pp db)
          ;; This is not catching the error raised by basic-save-buffer if
          ;; the destination is not writable.
          (condition-case error
              (let ((require-final-newline nil)
                    ;; get around write-file
                    (auto-save-default nil))
                (write-file ofile)
                (database-set-modified-p db nil))
            ;; Used to be `file-error', which didn't catch the error raised
            ;; by `basic-save-buffer' if the destination is not writable.
            (error
             (edb--G! :io-error-p t)
             ;; This must come before the buffer is killed.
             (db-warning "Error `%s' while writing buffer %s to file %s."
                         error (buffer-name orig-buf) ofile)))))
       ;; Don't use internal representation.
       (t
        (db-copy-buffer-local-variables orig-buf
                                        ;; We don't want to be *exactly*
                                        ;; like the data display buffer.
                                        'major-mode
                                        'buffer-read-only)
        (database-io-setup db)
        (let ((first-record t)
              (rsep (database-record-sepinfo db)))
          (when (sepinfo-pre-first-string rsep)
            (insert (sepinfo-pre-first-string rsep)))
          (if (database-write-region-from-record db)
              ;; Don't check separators for write-record-function, even for
              ;; recordsep; read-record-function's cleverness is unknown albeit
              ;; not unknowable.  Also, don't do substitution or quoting.
              (let ((record-sep-string (sepinfo-sep-string rsep))
                    (write-region-fn (database-write-region-from-record db))
                    ;; documented dynamic binding
                    (database db))
                (db-message "Writing database...")
                (db-maprecords (lambda (record)
                                 (if first-record
                                     (setq first-record nil)
                                   (insert record-sep-string))
                                 (funcall write-region-fn record))
                               db nil "Writing database...%d")
                (db-message "Writing database to disk..."))
            (db-write-intdelim db))
          (when (sepinfo-post-last-string rsep)
            (insert (sepinfo-post-last-string rsep)))

          (condition-case error
              (let ((require-final-newline nil)
                    ;; get around `write-file'
                    (auto-save-default nil))
                (write-file ofile)
                (database-set-modified-p db nil))
            ;; Used to be `file-error', which didn't catch the error raised
            ;; by `basic-save-buffer' if the destination is not writable.
            (error
             (edb--G! :io-error-p t)
             ;; This must come before the buffer is killed.
             (db-warning "Error `%s' while writing buffer %s to file %s."
                         error (buffer-name orig-buf) ofile))))))))
  ;; Update.
  (when (or (db-data-display-buffer-p)
            (db-summary-buffer-p))
    (force-mode-line-update)))


(defun db-write-intdelim (db)
  ;; Insert the records, fields separated by fieldsep and records separated by
  ;; recordsep, into the current buffer; it uses delimited format.  This is
  ;; called by `db-write-1', which arranges `unwind-protect' boundaries, calls
  ;; the -internal function that uses the proper output file layout, etc.
  (let* ((previous-point (point))
         (first-record t)
         (no-of-fields (length (database-fieldnames db)))
         (fsep (database-full-fieldsep-string db))
         (rsep (database-full-recordsep-string db))
         (sub-fsep (or (database-sub-fieldsep-string db) fsep))
         (sub-rsep (or (database-sub-recordsep-string db) rsep))
         (a->s-slice (db-rs-slice db 'edb--1rs-actual->stored))
         conftup)

    ;; Check the delimiters.
    (unless (database-acceptable-delimiter-p db sub-fsep)
      (setq sub-fsep (database-generate-delimiter nil)))
    (unless (database-acceptable-delimiter-p db sub-rsep)
      (setq sub-rsep (database-generate-delimiter nil)))

    (db-message "Writing database...")
    (db-maprecords
     (lambda (record)
       (dotimes (fno no-of-fields)
         (when (> fno 0) (insert sub-fsep))
         ;; This is `record-field-stored', inlined.
         (insert (let ((a->s (aref a->s-slice fno))
                       (val (aref record fno)))
                   (if a->s
                       (funcall a->s val)
                     val))))
       (insert sub-rsep))
     db nil "Writing database...%d")
    (db-message "Writing database...confirming")

    (narrow-to-region previous-point (point-max))
    (setq conftup (db-confirm-seps sub-fsep sub-rsep
                                   (length (database-fieldnames db))
                                   (edb--1D db :nrecords)))
    (if (or (db-conftup-bad-fsep conftup)
            (db-conftup-bad-rsep conftup))
        (progn
          (when (db-conftup-bad-fsep conftup)
            (db-warning "Unexpected field separator %s in data; %s."
                        (pp-to-string sub-fsep) "trying again")
            (setf (database-sub-fieldsep-string db)
                  (database-generate-delimiter db t)))
          (when (db-conftup-bad-rsep conftup)
            (db-warning "Unexpected record separator %s in data; %s."
                        (pp-to-string sub-rsep) "trying again")
            (setf (database-sub-recordsep-string db)
                  (database-generate-delimiter db t)))

          ;; We've chosen new separators; erase the work so far.
          (delete-region previous-point (point))

          ;; Call this function recursively.
          (db-write-intdelim db)
          ;; I've called this recursively; don't do any substitution
          ;; or quoting.
          )
      ;; Confirmation was OK:  correct number of field and record
      ;; separators found.

      ;; Put off adding the pre- and post- field strings until
      ;; after checking separators, as they may contain anomolous
      ;; field separators, for instance.

      ;; But do it before substitution so that all field pre- and
      ;; post- strings are treated identically.

      ;; The whole point of using io-separators is so they
      ;; appear exactly as the user specified, unaffected by
      ;; substitution.

      ;; But note that pre- and post- record strings will be added
      ;; later, after substitution.

      ;; Do the substitution, then, if field separators were changed in order
      ;; to prevent them from getting damaged by the substitution, convert
      ;; them back to the user-specified strings, which might contain
      ;; substrings that would have been substituted for in the previous
      ;; operation, had we not been careful.

      ;; Check that there are the correct number of fieldseps and recordseps
      ;; here; if wrong number, choose new fieldsep and/or recordsep and take
      ;; it from the top.

      (database-perform-substitutions db nil)

      (unless (equal sub-fsep fsep)
        (goto-char (point-min))
        (while (search-forward sub-fsep nil t)
          (replace-match fsep nil t)))
      (unless (equal sub-rsep rsep)
        (goto-char (point-min))
        (while (search-forward sub-rsep nil t)
          (replace-match rsep nil t)))

      ;; Now the buffer is ready to have the preceding and trailing junk
      ;; added and to be written to disk.

      ;; Convert from "simple" to "complex" form.
      (goto-char (point-min))
      (let ((pre-first (sepinfo-pre-first-string (database-field-sepinfo db))))
        (when pre-first
          (insert pre-first)))

      (goto-char (point-max))
      (if (and (db-skip-string-backward (or (sepinfo-pre-first-string
                                             (database-field-sepinfo db))
                                            ""))
               (db-skip-string-backward (or (sepinfo-sep-string
                                             (database-record-sepinfo db))
                                            "")))
          (delete-region (point) (point-max))
        (error "Didn't find expected trailing junk `%s' or `%s'."
               (sepinfo-pre-first-string (database-field-sepinfo db))
               (sepinfo-sep-string (database-record-sepinfo db)))))
    (widen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I/O utilities
;;;

(defun database-io-setup (db &optional norx)
  (flet ((bad (part s name)
              (error "Need a submatch to go with %s-regexp `%s' of %s."
                     part s name))
         (chk (si name sdef)
              ;; It's good that this setting of -regexp slots doesn't happen
              ;; until the last possible moment, when quotation-char and other
              ;; variables that these values depend on are already set to
              ;; their final values.

              ;; If the string is the empty string, then we must have just set
              ;; it that way, which means that we either also just set the
              ;; regexp to nil, or we set the regexp to something we care
              ;; about.  In either case don't mess further with the regexp.

              ;; The submatches could default to 0 if they're nil; but I want
              ;; to be more paranoid than that.
              (when (equal "" (sepinfo-pre-first-string si))
                (setf (sepinfo-pre-first-string si) nil))
              (cond ((sepinfo-pre-first-regexp si)
                     (unless (sepinfo-pre-first-regexp-submatch si)
                       (bad 'pre-first (sepinfo-pre-first-regexp si) name)))
                    ((and (sepinfo-pre-first-string si) (not norx))
                     (db-jam-si pre-first si)))

              ;; Don't test for sep-function because the sepinfo must be valid
              ;; for output as well as input.  On the other hand, we don't
              ;; need sep-string to be set if a wrfr function is in use, but
              ;; this function doesn't do any such checks.
              (when (or (not (sepinfo-sep-string si))
                        (and (equal "" (sepinfo-sep-string si))
                             (not (sepinfo-sep-regexp si))))
                (setf (sepinfo-sep-string si) sdef))
              (cond ((sepinfo-sep-regexp si)
                     (unless (sepinfo-sep-regexp-submatch si)
                       (bad 'sep (sepinfo-sep-regexp si) name)))
                    ((and (not (sepinfo-sep-function si)) (not norx))
                     (db-jam-si sep si)))

              (when (equal "" (sepinfo-post-last-string si))
                (setf (sepinfo-post-last-string si) nil))
              (cond ((sepinfo-post-last-regexp si)
                     (unless (sepinfo-post-last-regexp-submatch si)
                       (bad 'post-last (sepinfo-post-last-regexp si) name)))
                    ((and (sepinfo-post-last-string si) (not norx))
                     (db-jam-si post-last si)))))

    ;; Some of this information may already be correctly set (especially if
    ;; we're now writing), but just in case some of the database slots have
    ;; changed since reading.

    ;; When converting strings to regexps, must be careful to watch out for
    ;; substitution and quotation; don't get fooled.

    ;; When writing to the file, we take the previous version's local
    ;; variables section verbatim.

    ;; We're only setting the regexp variables.

    ;; When reading, we'll get the variables from the database, auxiliary,
    ;; and format files anew each time anyway.

    (chk (database-record-sepinfo db) "record" "\n")
    (chk (database-field-sepinfo db) "field" "\t")

    (edb--1D! db :substitution-no-no
              (apply 'concat
                     (mapcar (lambda (pair)
                               (concat (car pair) (cdr pair)))
                             (database-substitutions db))))))

(defmacro db-jam-si (afrag si)
  ;; AFRAG is a sepinfo accessor func name fragment (symbol).
  ;; SI is a variable (symbol) bound to a sepinfo.
  (let ((rx-acc (intern (format "sepinfo-%s-regexp" afrag)))
        (subrx-acc (intern (format "sepinfo-%s-regexp-submatch" afrag))))
    `(let ((str (,(intern (format "sepinfo-%s-string" afrag)) ,si)))
       (cond ((or (null str) (equal "" str))
              (setf (,rx-acc ,si) nil)
              (setf (,subrx-acc ,si) nil))
             (t
              (setf (,rx-acc ,si) (regexp-quote str))
              (setf (,subrx-acc ,si) 0))))))

(defun db-rfspec<-rftype (type)
  "Return the recordfieldspec associated with symbol TYPE."
  (let ((rv (gethash type (edb--G :1recordfieldtypes))))
    (when rv (if (symbolp rv)
                 (db-rfspec<-rftype rv)
               rv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution
;;;

;; When substitution is called, the rendered database is always in "simple"
;; format: rendered fields are separated by sub-fieldsep, and rendered records
;; (including the last one) are followed by sub-recordsep.

;; The "complicated" format includes pre-first-field at the beginning and
;; post-last-field (without record-sep-string or pre-first-field, which are
;; the other elements of sub-recordsep) at the end.

;; Simple format is nice because it requires no special-casing at the
;; beginning (because there's no gubbish there) or at the end (because
;; what's there is exactly what's between each pair of records).

(defun database-perform-substitutions (db backward)
  (when (database-substitutions db)
    (db-message "Substituting...")
    ;; Make replacements in the current buffer according to SUBS.
    ;; SUBS is list of pairs of strings; the cdr of each pair will be
    ;; substituted for the car, in order, unless optional argument BACKWARD is
    ;; non-nil, in which case the car is substituted for the cdr and the
    ;; substitutions are done in reverse order.
    ;;
    ;; Warn if any of the
    ;; substituted-in strings already appears in the buffer; such a
    ;; situation would make substitution, then unsubstitution, not yield
    ;; a result identical to the original buffer, since all instances of
    ;; the substituted-in string will be assumed on the reverse
    ;; substitution to have been the result of replacing a
    ;; substituted-for string.
    ;;
    ;; fixme: do all checking before any substitutions are done. --ttn
    (let ((subs (database-substitutions db))
          bef aft ambiguity ambiguities)
      (dolist (sub (if backward
                       (mapcar (lambda (pair)
                             (cons (cdr pair) (car pair)))
                           (reverse subs))
                     subs))
        (setq bef (car sub)
              aft (cdr sub))
        (goto-char (point-min))

        (when (search-forward aft nil t)
          (setq ambiguity sub)
          (goto-char (point-min)))

        (while (search-forward bef nil t)
          (replace-match aft nil t))

        ;; Don't complain if we didn't actually do any substitution.
        (when ambiguity
          (unless (= (point) (point-min))
            (push ambiguity ambiguities))
          (setq ambiguity nil)))

      (when ambiguities
        (error "Ambiguities: %s" ambiguities)))
    (db-message "Substituting...done")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution Utilities
;;;

;; A "confirmation tuple" -- aka "conftup" -- has the form:
;;   [FIELDSEP-BAD-P RECORDSEP-BAD-P NO-OF-RECORDS]

(defsubst db-conftup-bad-fsep (conftup) (aref conftup 0))
(defsubst db-conftup-bad-rsep (conftup) (aref conftup 1))
(defsubst db-conftup-reccount (conftup) (aref conftup 2))

(defun db-confirm-seps (fsep rsep nfields nrecords)
  ;; Return a conftup.
  ;; Assume that recordsep appears at the end of the database as well.

  ;; here "-m" means "matches"
  (let* ((fsep-m (progn (goto-char (point-min))
                        (db-how-many-string-overlapping fsep)))
         (rsep-m (progn (goto-char (point-min))
                        (db-how-many-string-overlapping rsep)))
         (goal-fsep-m (and nrecords (* nrecords (1- nfields))))
         (goal-rsep-m nrecords))
    (if (equal fsep rsep)
        (progn
          (if nrecords
              (if (= rsep-m (+ goal-rsep-m goal-fsep-m))
                  (vector nil nil nrecords)
                (vector t t nil))
            ;; Remember that fsep = rsep when reading this code.
            (if (zerop (% rsep-m nfields))
                (vector nil nil (/ rsep-m nfields))
              (if (zerop (% (1+ rsep-m) nfields))
                  ;; Field separator at the end of data was misinterpreted
                  ;; as a record separator, so no record separator was added.
                  (progn (goto-char (point-max))
                         (insert rsep)
                         (vector nil nil (/ (1+ rsep-m) nfields)))
                (vector t t (/ rsep-m nfields))))))

      ;; fsep and rsep unequal; see if one is a substring of the other.

      ;; At least one of these must be zero.
      (let ((f-in-r (db-how-many-substring-overlapping fsep rsep))
            (r-in-f (db-how-many-substring-overlapping rsep fsep)))
        (if nrecords
            (progn
              ;; At most one of these is nonzero, so cond is OK.
              (cond ((> f-in-r 0)
                     (incf goal-fsep-m (* goal-rsep-m f-in-r)))
                    ((> r-in-f 0)
                     (incf goal-rsep-m (* goal-fsep-m r-in-f))))
              (vector (not (= fsep-m goal-fsep-m))
                      (not (= rsep-m goal-rsep-m))
                      nrecords))
          (incf nfields f-in-r)
          (let ((apparent-records (if (= 1 nfields)
                                      rsep-m
                                    (/ fsep-m (1- nfields)))))
            (setq goal-rsep-m
                  (if (zerop apparent-records)
                      1
                    (* apparent-records
                       (1+ (* r-in-f fsep-m)))))
            (vector (or
                     ;; Wrong field count; some record has too many or few.
                     (not (= fsep-m (* apparent-records (1- nfields))))
                     ;; too many fseps compared to rseps
                     (< rsep-m goal-rsep-m))
                    ;; too many rseps compared to fseps
                    (< goal-rsep-m rsep-m)
                    apparent-records)))))))


(defun database-acceptable-delimiter-p (db delimiter)
  ;; Return t iff no characters of DELIMITER appear in `:substitution-no-no'.
  (when delimiter
    (let ((result t)
          (idx 0)
          (len (length delimiter))
          (no-no (edb--1D db :substitution-no-no)))
      (while (and result (< idx len))
        (if (db-find-char (elt delimiter idx) no-no)
            (setq result nil)
          (incf idx)))
      result)))

(defun database-generate-delimiter (db &optional checkp)
  (let ((fsep (or (database-sub-fieldsep-string db)
                  (database-full-fieldsep-string db)))
        (rsep (or (database-sub-recordsep-string db)
                  (database-full-recordsep-string db)))
        (string (make-string 1 0))
        (c 0)
        rv)
    (while (and (< c 256) (not rv))
      (aset string 0 c)
      (if (and (database-acceptable-delimiter-p db string)
               (not (and checkp
                         (progn
                           (goto-char (point-min))
                           (search-forward string nil t))))
               (not (db-find-char c fsep))
               (not (db-find-char c rsep)))
          (setq rv string)
        (incf c)))
    (or rv (error "I can't find an acceptable delimiter!"))))

;;; db-file-io.el ends here
;;; db-interfa.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Commands for operating on the current database.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar dbc-database nil
  "The database associated with this format.
This variable is also set in the summary format.")

(defun dbc-set-hide-p (value)
  "Enable hiding if VALUE is non-nil, otherwise disable it.
This is done in both the data display buffer and summary buffer."
  (edb--S! :hide-p value)
  (let ((other (or (edb--S :data-display-buffer)
                   (edb--S :sumbuf))))
    (when other (with-current-buffer other (edb--S! :hide-p value)))))

;;; Etc.

(put (quote db-new-record-function) (quote safe-local-variable) (quote edb-true))
(defvar db-new-record-function nil
  "Function called on empty records before they're inserted in the database.
Takes two arguments, the record and the database.  This variable is set
only in the data display buffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps
;;;

(defvar database-view-mode-map nil
  "Keymap for database data display buffer in view mode.")

(unless database-view-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    (suppress-keymap m)
    (mapc (lambda (key-def)
            (define-key m (let ((key (car key-def)))
                            (if (consp key)
                                (concat meta (car key))
                              key))
              (cdr key-def)))
          '(("t" . toggle-truncate-lines)
            ("+" . db-additional-data-display-buffer)

            ;; Moving around in the database
            ("n"      . db-next-record)
            ("p"      . db-previous-record)
            ("<"      . db-first-record)
            (">"      . db-last-record)
            (("<")    . db-first-record)
            ((">")    . db-last-record)
            ("j"      . db-jump-to-record)
            (" "      . db-next-screen-or-record)
            ("\177"   . db-previous-screen-or-record)
            (("n")    . db-next-record-ignore-hiding)
            (("p")    . db-previous-record-ignore-hiding)
            (("\C-n") . db-next-marked-record)
            (("\C-p") . db-previous-marked-record)

            ;; Changing to edit mode
            ("\t"   . db-first-field)
            (("\t") . db-last-field)
            ("\C-n" . db-first-field)
            ("\C-p" . db-last-field)
            ("e"    . db-first-field)
            ;; These could be db-first-field and db-last-field, but that
            ;; wouldn't fit in: nowhere else are these keystrokes
            ;; inter-field-movement keystrokes.
            ("\C-f" . undefined)
            ("\C-b" . undefined)
            ("\C-a" . undefined)
            ("\C-e" . undefined)
            ;; ("v" . db-view-mode)
            ("\C-v" . db-scroll-up)
            ;; In view-mode, we're at the top of the buffer (not after
            ;; db-next-screen).
            (("v") . db-scroll-down)

            ;; Undoing changes
            ("\C-xu" . db-revert-record)
            ;;("u"   . db-revert-record)
            ("\C-xr" . db-revert-database)

            ;; Adding and removing records
            ("a" . db-add-record)
            ("i" . db-add-record)
            ("d" . db-delete-record)
            ("k" . db-delete-record)
            ("y" . db-yank-record)
            ("o" . db-output-record-to-db)
            ("c" . db-copy-record)

            ;; Sorting
            ("S" . db-sort)

            ;; Searching commands
            (("S")  . db-search)
            (("s")  . db-search)
            ("s"    . db-search)
            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            ;; Exiting database mode
            ("q" . db-quit)
            ("x" . db-kill-buffer)
            ("X" . db-kill-all-buffers)

            ("m" . db-mark-record)

            ("?" . describe-mode)

            ;; Gross key bindings.
            ("O"      . db-hide-record)
            (("o")    . db-hiding-toggle)
            (("O")    . db-hiding-set)
            (("\C-o") . db-toggle-show-hidden-records)

            ("F" . dbf-set-summary-format)
            ("D" . db-summary)          ; mnemonic for Directory
            ("h" . db-summary)          ; mnemonic for Headers
            ("H" . db-summary)          ; mnemonic for Headers

            ("r"  . db-report)
            ("\r" . db-accept-record)))
    (setq database-view-mode-map m)))


(defvar database-edit-mode-map nil
  "Keymap for database data display buffer in edit mode.")

(unless database-edit-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    ;; Obviously don't do suppress-keymap on this one; we want to be
    ;; able to edit.  The view-mode commands should be available via C-c
    ;; and many (such as next-record) available via M- commands as well,
    ;; espcially those not ordinarily bound in text mode (eg M-n and
    ;; M-p).
    (mapc (lambda (key-def)
            (define-key m
              (let ((key (car key-def)))
                (if (consp key)
                    (concat meta (car key))
                  key))
              (cdr key-def)))
          '(;; Exiting edit mode
            ("\C-c\C-c" . db-view-mode)

            ;; Undoing changes
            ("\C-xU" . db-revert-field)

            ;; Moving from record to record
            (("n") . db-next-record)
            (("p") . db-previous-record)

            ;; Moving from field to field
            ("\t"   . db-next-field)
            (("\t") . db-previous-field)
            (("<")  . db-first-field)
            ((">")  . db-last-field)
            ("\C-v" . db-scroll-up)
            (("v")  . db-scroll-down)

            ;; Movement within a field
            ("\C-n" . db-next-line-or-field)
            ("\C-p" . db-previous-line-or-field)
            ;; almost-the-same-as-before commands
            ("\C-f" . db-forward-char)
            ("\C-b" . db-backward-char)
            (("f")  . db-forward-word)
            (("b")  . db-backward-word)
            ("\C-a" . db-beginning-of-line-or-field)
            ("\C-e" . db-end-of-line-or-field)

            ;; Editing a field
            ;;insertion
            ("\r"     . db-newline)
            ("\n"     . db-newline)
            ("\C-o"   . db-open-line)
            ;;deletion
            ("\C-d"   . db-delete-char)
            ("\177"   . db-backward-delete-char)
            (("d")    . db-kill-word)
            (("\177") . db-backward-kill-word)
            ("\C-k"   . db-kill-line)
            (("k")    . db-kill-to-end)
            ("\C-w"   . db-kill-region)
            (("w")    . db-copy-region-as-kill)

            ;; Other commands
            (("s")   . db-search-field)
            ;;(("S") . db-search-field)

            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            (("?") . db-field-help)))
    (setq database-edit-mode-map m)))


;;; Bindings for both keymaps

(mapc (lambda (&optional key-def)
        (apply 'define-key database-view-mode-map key-def)
        (apply 'define-key database-edit-mode-map key-def))
      '( ;; Saving the database
        ("\C-x\C-s" db-save-database)
        ("\C-x\C-w" db-write-database-file)

        ;; Toggling modifiable-p
        ("\C-x\C-q" db-toggle-modifiable-p)

        ;; Wipe out dangerous commands
        ("\C-xn" undefined)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

;; Menus by Alastair Burt <burt@dfki.uni-kl.de>,
;;          Michael Ernst <mernst@theory.lcs.mit.edu>
;;          John Overton <overton@cs.uchicago.edu>

(defvar database-view-mode-menu
  '("Database"
    "VIEW Mode:"
    ["Edit"            db-first-field                 t]
    ["Report"          db-report                      t]
    ["Summary"         db-summary                     t]
    "---"
    ["Revert  record" db-revert-record t]
    ["Accept  record" db-accept-record t]
    ["Add     record" db-add-record t]
    ["Copy    record" db-copy-record t]
    ["Delete  record" db-delete-record t]
    ["Output  record" db-output-record-to-db t]
    ["Mark    record" db-mark-record t]
    ["Hide    record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    ("Motion"
     ["jump to"  db-jump-to-record t]
     ["first" db-first-record   t]
     ["last"  db-last-record    t]
     "---"
     ["next"               db-next-record                     t]
     ["next (screen)"      db-next-screen-or-record           t]
     ["next (marked)"      db-next-marked-record              t]
     ["next (ignore hiding)" db-next-record-ignore-hiding     t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen)"      db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ignore hiding)" db-previous-record-ignore-hiding t]
     )
    "----"
    ["Create Report" db-report t]
    ["Toggle Hiding" db-hiding-toggle t]
    ["Summary" db-summary t]
    "----"
    ["Edit Mode" db-first-field t]
    "----"
    ["Sort    database" db-sort t]
    ["Revert  database" db-revert-database t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    ["Internal Layout" db-toggle-internal-file-layout t]
    "----"
    ["Quit" db-quit t]
    )
  "Menu for Database View mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-view-mode-map
  "ignored-doc-string"
  database-view-mode-menu)


(defvar database-edit-mode-menu
  '("Database"
    "EDIT Mode:"
    ["View mode"        db-view-mode      t]
    ["Report"           db-report         t]
    ["Summary"          db-summary        t]
    ["Summary   subset" db-summary-subset t]
    "---"
    ["Revert    record"  db-revert-record     t]
    "---"
    ["Revert     field"  db-revert-field   t]
    ["Help    on field" db-field-help t]
    ["Search  in field" db-search-field t]
    "---"
    ("Motion"
     ["Next       field"  db-next-field     t]
     ["Prev       field"  db-previous-field t]
     ["Last       field"  db-last-field     t]
     ["First      field"  db-first-field    t]
     ["Next      record" db-next-record t]
     ["Previous  record" db-previous-record t])
    "---"
    ["Revert  database" db-revert-database t]
    ["Search  database"  db-search-field        t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    "---"
    ["Quit" db-quit t]
    )
  "Menu for Database Edit mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-edit-mode-map
  "ignored-doc-string"
  database-edit-mode-menu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help
;;;

(defun db-set-field-help (db &rest spec)
  "Set field-specific help info for database DB from F1 H1, ...
F1, F2, ... should be field names (symbols) or field numbers,
while H1, H2, ... should be either a string or a form to be
evaluated that results in a string.  See `db-field-help'.

This should be called after the field names have been set up.

\(fn DB F1 H1 ...)"
  (let* ((len (length (database-fieldnames db)))
         (fh (or (edb--1D db :field-help)
                 (edb--1D! db :field-help (make-vector len nil))))
         (nm2no (edb--1D db :nm2no))
         field help-info)
    (while spec
      (setq field (pop spec)
            help-info (pop spec))
      (aset fh (if (symbolp field)
                   (gethash field nm2no)
                 field)
            help-info))))

(defun db-field-help ()
  "Display help for the current field in the echo area.
Help info comes from two places: field-specific help info, and
recordfieldtype-specific help info.  In both cases, if the info
is a string, display it.  Otherwise, `eval' it as a Lisp
expression and display the result (which should be a string).
When both sources provide the help info, the display the
field-specific info followed by two newlines followed by the
recordfieldtype-specific info."
  (interactive)
  (unless (edb--S :this-ds)
    (error "Not on a field."))
  (flet ((try (x) (when x
                    (if (stringp x)
                        (list "%s" x)
                      (condition-case err
                          (list "%s" (eval x))
                        (error
                         (list "Help form: %S\nfailed with error: %S"
                               x err)))))))
    (let* ((fidx (edb--1ds-record-index (edb--S :this-ds)))
           (one (let ((fh (edb--1D dbc-database :field-help)))
                  (and fh (try (aref fh fidx)))))
           (two (try (aref (db-rs-slice dbc-database 'edb--1rs-help-info)
                           fidx))))
      (if (or one two)
          (apply 'message (concat (car one) (and one two "\n\n") (car two))
                 (append (cdr one) (cdr two)))
        (message "No help available for `%s'."
                 (db-fname<-fno fidx dbc-database))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quitting
;;;

;; These work in both the data display buffer and the summary buffer, for
;; folks who enjoy spending most of their time in the summary who will rebind
;; its keystrokes to call these functions instead of dbs-exit (for instance).

(defun db-quit ()
  "Quit editing the database for now; bury its buffers."
  (interactive)
  ;; Bury the data display and summary buffers.
  (let (dd sum)
    (db-in-data-display-buffer
      (setq dd  (current-buffer)
            sum (edb--S :sumbuf)))
    (when dd
      (delete-windows-on dd)
      (bury-buffer dd))
    (when sum
      (delete-windows-on sum)
      (bury-buffer sum))))

(defun db-kill-buffer (&optional noask)
  "Kill this data display buffer and any associated summary buffer.
Offer to save any changes.  Prefix arg means don't offer.
See also `db-kill-all-buffers'."
  (interactive "P")
  (unless noask
    (db-save-database t))
  (edb--S! :discard-changes! t)
  (kill-buffer nil))

(defun db-kill-all-buffers (&optional noask)
  "Kill all buffers associated with the current database.
Offer to save any changes.  Prefix arg means don't offer.
See also `db-kill-buffer'."
  (interactive "P")
  (unless noask
    (db-save-database t))
  (dolist (ddb (edb--1D dbc-database :ddbufs))
    (with-current-buffer ddb
      (edb--S! :discard-changes! t)
      (kill-buffer nil))))

(defun db-exit (&optional kill)
  "Be done with the database; like `db-quit', but offers to save any changes.
With prefix argument, kills the data display buffer, and the database, if that
was its only data display buffer."
  (interactive "P")
  (db-save-database t)
  (if kill
      (db-kill-all-buffers t)
    (db-quit)))

(defun db-kill-buffer-hook ()
  (when (edb--rget '(:edb1 :bprops) (current-buffer))
    (cond ((db-summary-buffer-p)
           (with-current-buffer (edb--S :data-display-buffer)
             (edb--S! :sumbuf nil)))
          ((db-data-display-buffer-p)
           (let ((left (remq (current-buffer)
                             (edb--1D dbc-database :ddbufs)))
                 buf)
             (unless (edb--S :discard-changes!)
               (when (and (or (edb--S :utkmodp)
                              (dbf-this-field-modified-p))
                          (y-or-n-p "Commit current record? "))
                 (dbf-process-current-record-maybe t))
               (db-save-database t left))
             (when (setq buf (edb--S :sumbuf))
               (delete-windows-on buf)
               (kill-buffer buf))
             (if left
                 (edb--1D! dbc-database :ddbufs left)
               ;; slightly suboptimal from maintenance pov --ttn
               (when (setq buf (edb--S :ddb-spec))
                 (kill-buffer buf))
               (edb--meta1D dbc-database :forget)))))
    (edb--rforget '(:edb1 :bprops) (current-buffer))))

(defun db-save-some-buffers (&optional quietly exiting)
  "Save some modified databases and file-visiting buffers.
Asks user about each one.  With argument, saves all with no questions."
  (interactive "P")
  (db-save-some-databases quietly)
  (save-some-buffers quietly exiting))

;; This isn't quite right because it should modify the ???.
(defun db-save-some-databases (&optional quietly)
  "Save some modified databases.  Asks user about each one.
With argument, saves all with no questions."
  (interactive "P")
  (let (buffers)
    (dolist (db (edb--1all-known-databases))
      (setq buffers (edb--1D db :ddbufs))
      (when buffers
        (dolist (buf buffers)
          (db-in-buffer buf (dbf-process-current-record-maybe t)))
        (db-in-buffer (car buffers) (db-save-database (not quietly)))
        (dolist (buf buffers)
          (with-current-buffer buf
            (force-mode-line-update)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File I/O
;;;

;;;###autoload
(defun db-find-file (filename &optional promptp)
  "Read a database from FILENAME; prompts when called interactively.
If the database file doesn't specify a format and the format file can't be
inferred from FILENAME, the user is prompted for it too.
The user is always prompted for the format if prefix argument
PROMPTP is non-nil.
If the database is already read in and PROMPTP is nil, the existing
database buffer is merely selected.
When called non-interactively, argument PROMPTP may be a string, the
name of a format file to use."
  (interactive "fDatabase file: \nP")
  ;; fixme: check whether the file is currently read in. --ttn
  (setq filename (expand-file-name filename))
  (let ((format-file (when (stringp promptp) promptp))
        database data-display-buffer)
    (when (stringp promptp)
      (setq promptp nil))

    (unless promptp
      (setq database (db-find-read-in-database filename))
      (when (and database (not (edb--1D database :ddbufs)))
        (setq database nil))
      ;; Find an appropriate data display buffer
      (when (and database format-file)
        (let ((bufs (edb--1D database :ddbufs))
              ddb-format-file)
          (while bufs
            (if (db-same-file-p format-file
                                (with-current-buffer (car bufs)
                                  (edb--S :format-file)))
                (setq data-display-buffer (car bufs)
                      bufs nil)
              (setq bufs (cdr bufs)))))))
    (unless database
      ;; Either promptp is non-nil, or we couldn't find an
      ;; appropriate read-in-database.
      (setq database (db-read-database-file filename format-file promptp)))
    (unless data-display-buffer
      (setq data-display-buffer (car (edb--1D database :ddbufs))))
    (switch-to-buffer data-display-buffer)
    (assert (eq (current-buffer) (variable-binding-locus 'dbc-database)))
    (setq dbc-database database))
  (db-first-record))

(defun db-find-read-in-database (filename)
  ;; Return the database most recently read in from FILENAME, or nil.
  (let ((db-ls (edb--1all-known-databases))
        result db maybe)
    (while (and (not result) db-ls)
      (setq db (car db-ls)
            maybe (edb--1D db :file)
            result (when (and (db-same-file-p
                               filename (if (equal "(inherent)" maybe)
                                            (get-text-property 0 :file maybe)
                                          maybe))
                              (car (edb--1D db :ddbufs)))
                     db)
            db-ls (cdr db-ls)))
    result))

(defun db-revert-database ()
  "Replace the database with the data on disk.
This undoes all changes since the database was last saved."
  (interactive)
  (when (yes-or-no-p (format "Revert database from file %s? "
                             (edb--1D dbc-database :file)))
    (let ((database dbc-database))
      (with-temp-buffer
        (insert-file-contents (edb--1D database :file) nil)
        (unless (setq database (db-read-database-internal-file-layout))
          (error "File no longer contains a database"))
        (db-read-database-file-helper database))

      (mapc (lambda (buf)
              (with-current-buffer buf
                ;; abandon any changes
                (dbf-set-this-field-modified-p nil)
                (edb--S! :utkmodp nil)
                (db-jump-to-record (edb--S :index) nil)))
            (edb--1D database :ddbufs))

      (db-message "Reverted database from disk"))))

(defun db-save-database (&optional query quietly)
  "Save the database to disk in the default save file.
Any changes to the current record are processed first.
The default save file is the file it was last saved to or read from.
If optional arg QUERY is specified, the user is asked first.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  ;; fixme: check if file is more recent than buffer. --ttn
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (if (edb--1D dbc-database :modp)
        (when (or (not query)
                  (yes-or-no-p (concat "Save database "
                                       (database-print-name dbc-database)
                                       "? ")))
          (db-write-database-file (edb--1D dbc-database :file) quietly))
      (unless quietly
        (let ((name (database-print-name dbc-database)))
          (when (and (stringp name)
                     (or (string= "" name)
                         (string= "Unnamed Database "
                                  (substring name 0 (min 17 (length name))))))
            (setq name nil))
          (db-message "No changes need to be saved%s"
                      (if name
                          (format " in %S" name)
                        "")))))))

(defun db-write-database-file (&optional filename quietly)
  "Save the database to file FILENAME; it becomes the default save file.
Any changes to the current record are processed first.
If FILENAME is not specified, the user is prompted for it.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  ;; Do this before asking for the filename.
  (dbf-process-current-record-maybe t)
  ;; Save even if the database is not modified.
  (unless filename
    (setq filename (read-file-name
                    (format "Save database %s into file: "
                            (database-print-name dbc-database)))))
  (unless (equal filename (edb--1D dbc-database :file))
    (edb--1D! dbc-database :file filename)
    ;; Rename the buffer.
    (rename-buffer (generate-new-buffer-name
                    (file-name-nondirectory filename))))
  (when (equal "(inherent)" filename)
    (setq quietly t)
    (db-message "Saving %s..." (buffer-name)))
  (let ((msg (format "Saving database to file %s..." filename)))
    (unless quietly (db-message "%s" msg))
    (db-write-1)
    (unless (or quietly (edb--G :io-error-p))
      (db-message "%sdone" msg))))

(defun db-toggle-internal-file-layout (&optional arg)
  "Toggle whether the database will be saved in EDB's internal file layout.
With a nonzero prefix argument, set it to use internal file layout.
With a zero prefix argument, set it not to use internal file layout."
  (interactive "P")
  (let ((v (if arg
               (not (zerop (prefix-numeric-value arg)))
             (not (edb--1D dbc-database :togp)))))
    (edb--1D! dbc-database :togp v)
    (when (interactive-p)
      (message "Use of internal file layout now %sabled"
               (if v "en" "dis")))))

(defun db-toggle-modifiable-p (&optional arg)
  "Toggle whether the database may be modified by the user.
With a nonzero prefix argument, set it modifiable.
With a zero prefix argument, set it non-modifiable."
  (interactive "P")
  (let ((modifiable-p (if arg
                          (not (zerop (prefix-numeric-value arg)))
                        (not (edb--1D dbc-database :modifiable-p)))))
    (edb--1D! dbc-database :modifiable-p modifiable-p)
    ;; fixme: handle mods in the presence of nil `modifiable-p'. --ttn
    (db-in-data-display-buffer
      (when (eq 'database-edit-mode major-mode)
        (setq buffer-read-only (not modifiable-p)))
      (set-buffer-modified-p (buffer-modified-p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record selection
;;;

;; This is the common body of `db-next-record' and `db-jump-to-record'.
(defmacro dbf-goto-record-internal (&rest record-selector-body)
  (declare (indent 0) (debug body))
  `(progn
     ;; NOTE: This line will cause problems if some hook function
     ;;       deliberately raises an error (like Joe Wells' do).
     (db-view-mode)
     (dbf-process-current-record-maybe nil)
     ,@record-selector-body
     (edb--S! :original (aref (edb--1D dbc-database :vov)
                              (1- (edb--S :index))))
     (db-display-record (dbf-displayed-record) t)))

(defun db-jump-to-record (arg &optional respect-hiding)
  "Show the database's ARGth record.  If ARG is a record, use its index.
Hiding is ignored unless optional argument RESPECT-HIDING is specified."
  ;; NOTE: Respect and ignorance have opposite nature.
  (interactive "NJump to record number: ")
  (unless (integerp arg)
    (let ((vov (edb--1D dbc-database :vov))
          (rno (edb--1D dbc-database :nrecords))
          (i 0))
      ;; linear search, blech
      (while (and (< i rno) (not (integerp arg)))
        (when (eq (aref vov i) arg)
          (setq arg (1+ i)))
        (incf i))
      (unless (integerp arg)
        ;; make `db-select-record' throw out-of-range error
        (setq arg -1))))
  (db-in-data-display-buffer
    (dbf-goto-record-internal
      (db-select-record arg (not respect-hiding))))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format)))

(defun db-first-record (&optional ignore-hiding)
  "Show the database's first record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
         (db-jump-to-record 1 (not ignore-hiding)))
        ((db-summary-buffer-p)
         ;; fixme: handle hiding. --ttn
         (goto-char (point-min))
         (db-jump-to-point))
        (t
         (error "db-first-record called in wrong context."))))

(defun db-last-record (&optional ignore-hiding)
  "Show the database's last record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
         (db-jump-to-record (edb--1D dbc-database :nrecords)
                            (not ignore-hiding)))
        ((db-summary-buffer-p)
         (goto-char (point-max))
         (db-jump-to-point))
        (t
         (error "db-last-record called in wrong context"))))

(defun db-next-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth next record.
In that record, go to the current field, if any."
  (interactive "p")
  (when (db-summary-buffer-p)
    (dbs-synch-format-with-summary))
  (db-in-data-display-buffer
    (let ((this-fidx (edb--S :this-fidx)))
      (dbf-goto-record-internal
        (db-select-next-record arg ignore-hiding markedp))
      ;; If in edit mode, stay in edit mode in the same field.
      (when (and this-fidx (edb--S :stay-in-edit-mode-p))
        (db-move-to-field-exact this-fidx))))
  (when (db-summary-buffer-p)
    (let ((index (with-current-buffer (edb--S :data-display-buffer)
                   (edb--S :index))))
      ;; This might not be right, depending on what records are summarized.
      (dbs-forward-record (- index (edb--S :index)))
      (dbs-set-index index))))

(defsubst db-previous-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth previous record.
In that record, go to the current field, if any."
  (interactive "p")
  (db-next-record (- arg) ignore-hiding markedp))

(defsubst db-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record arg t))

(defsubst db-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record-ignore-hiding (- arg)))

(defsubst db-next-marked-record (arg)
  "Go to the ARGth next marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-record arg nil t))

(defsubst db-previous-marked-record (arg)
  "Go to the ARGth previous marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-marked-record (- arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from record to record (setting :index)
;;;

;; These don't display, but they do set `:index'.

;; Don't forget that when moving off a record, must check whether it has
;; been modified and, if so, call an update function.

(defun dbc-set-index (index &optional record)
  (unless record
    (setq record (aref (edb--1D dbc-database :vov) (1- index))))
  (edb--S! :index index)
  (edb--S! :index-fraction
           (let ((frac (format "%s%d/%d"
                               (if (edb-tagp
                                    (edb-tag :markedp dbc-database)
                                    record)
                                   "+" "")
                               index
                               (edb--1D dbc-database :nrecords))))
             (if (and (edb--S :hide-p) (edb-tagp
                                        (edb-tag :hiddenp dbc-database)
                                        record))
                 (concat "[" frac "]")
               frac))))

(defun db-select-next-record (n &optional ignore-hiding markedp)
  ;; Set new `:index' to point the Nth following, or if N is negative,
  ;; the -Nth preceding, record.  Respect `:hide-p' and `:wraparound'.
  (interactive "p")
  (let* ((up (< 0 n))
         (hidep (and (edb--S :hide-p) (not ignore-hiding)))
         (wrapp (or (eq t (edb--S :wraparound))
                    (and (eq 'delay (edb--S :wraparound))
                         (eq :failed last-command))))
         (idx (edb--S :index))
         (vov (edb--1D dbc-database :vov))
         (rno (edb--1D dbc-database :nrecords))
         (new (flet ((new-idx (delta) (1+ (% (+ rno delta -1 idx) rno))))
                (if (or hidep markedp)
                    ;; slow path
                    (let ((sign (if up 1 -1))
                          (good nil)
                          (stop (cond (wrapp idx)
                                      (up rno)
                                      (t 1)))
                          (htag (and hidep (edb-tag :hiddenp dbc-database)))
                          (mtag (and markedp (edb-tag :markedp dbc-database)))
                          record)
                      (while (not (or (zerop n)
                                      (if (not wrapp)
                                          (prog1 (= idx stop)
                                            (setq idx (new-idx sign)))
                                        (setq idx (new-idx sign))
                                        (= idx stop))))
                        (setq record (aref vov (1- idx)))
                        (unless (or (and htag (edb-tagp htag record))
                                    (and mtag (not (edb-tagp mtag record))))
                          (setq good idx)
                          (decf n sign)))
                      (or good (edb--S :index)))
                  ;; fast path
                  (new-idx (cond (wrapp n)
                                 (up (min n (- rno idx)))
                                 (t (max n (- 1 idx)))))))))
    (dbc-set-index (if (/= new (edb--S :index))
                       new
                     (setq this-command :failed)
                     new)
                   (aref vov (1- new)))
    (when (eq :failed this-command)
      (message "%s record" (if up "Last" "First")))))

(defun db-select-first-record (&optional ignore-hiding)
  ;; Select first record.  Does no display.
  ;; If hiding is in effect, select the first unhidden record, unless
  ;; optional argument IGNORE-HIDING is non-nil.
  (interactive)
  (if (and (edb--S :hide-p)
           (not ignore-hiding)
           (edb-tagp (edb-tag :hiddenp dbc-database)
                     (aref (edb--1D dbc-database :vov) 0)))
      (progn
        (edb--S! :index 1)
        (db-select-next-record 1))
    (dbc-set-index 1)))

(defun db-select-record (index &optional ignore-hiding)
  ;; Select record at INDEX.  Throw error if INDEX out of range.
  ;; If that record is hidden, select the first following non-hidden record,
  ;; unless optional argument IGNORE-HIDING is non-nil.
  (interactive "nRecord number: ")
  (let ((rno (edb--1D dbc-database :nrecords)))
    (unless (and (<= 1 index) (<= index rno))
      (error "Record number %d out of range 1..%d" index rno)))
  (db-select-first-record ignore-hiding)
  (db-select-next-record (1- index) ignore-hiding))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid field/record movement commands
;;;

(defun db-next-screen-or-record (arg)
  "Go to the ARGth next screenful of this display, or to the ARGth
next record, if this is the last screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the next record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
         (dbf-next-screen-or-record arg))
        ((db-summary-buffer-p)
         (let ((w (get-buffer-window (edb--S :data-display-buffer))))
           (if w
               (progn
                 (with-selected-window w
                   (dbf-next-screen-or-record arg))
                 (dbs-synch-summary-with-format))
             (db-next-record arg))))))

(defun dbf-next-screen-or-record (arg)
  (let ((pmax (point-max)))
    (if (= pmax (window-end))
        (db-next-record arg)
      (while (and (> arg 0) (not (= pmax (window-end))))
        (scroll-up nil)
        (decf arg)))))

(defun db-previous-screen-or-record (arg)
  "Go to the ARGth previous screenful of this display, or to the ARGth
previous record, if this is the first screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the previous record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
         (dbf-previous-screen-or-record arg))
        ((db-summary-buffer-p)
         (let ((w (get-buffer-window (edb--S :data-display-buffer))))
           (if w
               (progn
                 (with-selected-window w
                   (dbf-previous-screen-or-record arg))
                 (dbs-synch-summary-with-format))
             (db-previous-record arg))))))

(defun dbf-previous-screen-or-record (arg)
  (let ((pmin (point-min)))
    (if (= pmin (window-start))
        (db-previous-record arg)
      (while (and (> arg 0) (not (= pmin (window-start))))
        (scroll-down nil)
        (decf arg))
      (when (= pmin (window-start))
        (goto-char (point-min))))))


(defun db-beginning-of-field-or-record ()
  "Move to the beginning of this field.
If already at its beginning, move to the first field."
  (interactive)
  (if (= (point) (edb--S :fbeg))
      (db-first-field)
    (db-beginning-of-field)))

(defun db-end-of-field-or-record ()
  "Move to the end of this field; if at its end, to the last field."
  (interactive)
  (if (= (point) (dbf-this-field-end-pos))
      (db-last-field)
    (db-end-of-field)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and deleting records
;;;

(defun db-add-record (&optional append)
  "Add a new record to the database immediately before the current record.
Prefix arg APPEND means to add after the last record, instead.  After adding,
move point to the new record's first field and switch to Database Edit mode."
  (interactive "P")
  (when (db-summary-buffer-p)
    (pop-to-buffer (edb--S :data-display-buffer)))
  (let* ((nfields (length (database-fieldnames dbc-database)))
         (defaults-slice (db-rs-slice dbc-database 'edb--1rs-default-value))
         (new (make-vector nfields nil)))
    (dotimes (fno nfields)
      (aset new fno (aref defaults-slice fno)))
    (when db-new-record-function
      (funcall db-new-record-function new dbc-database))
    (database-add-record new dbc-database (unless append
                                            (edb--S :index))))
  (edb--S! :index (if append
                      (edb--1D dbc-database :nrecords)
                    (1+ (edb--S :index))))
  ;; Why doesn't this need to be (dbc-set-modified-p t)?
  (database-set-modified-p dbc-database t)
  ;; Probably unnecessary, as `database-add-record' has done the trick.
  (dbf-set-summary-out-of-date-p)
  ;; fixme: update incrementally instead of fully. --ttn
  (let ((sumbuf (edb--S :sumbuf)))
    (when (and sumbuf (get-buffer-window sumbuf))
      (db-in-buffer sumbuf
        (dbs-synch-summary-with-format))))
  (db-previous-record (if append 0 1))
  (db-message "%sed a new record" (if append "Append" "Insert"))
  ;; Begin editing the new record.  Don't use `db-edit-mode'.
  (db-first-field))

(defun db-delete-record (&optional force)
  "Remove the current record from the database.
With a prefix argument, doesn't verify."
  ;; fixme: handle "no more records" situation --ttn
  (interactive "P")
  (when (or force (y-or-n-p "Delete this record? "))
    (let ((cur (edb--S :index))
          (vov (edb--1D dbc-database :vov))
          (rno (edb--1D dbc-database :nrecords)))
      ;; stash
      (edb--1D! dbc-database :deleted-record (aref vov (1- cur)))
      ;; shift down and clear highest
      (do ((i (1- cur) (1+ i)))
          ((= (1- rno) i))
        (aset vov i (aref vov (1+ i))))
      (aset vov (decf rno) nil)
      (edb--1D! dbc-database :nrecords rno)
      (edb--S! :index (if (= 1 cur) ; ugh
                          rno
                        (1- cur))))
    ;; update the summary directly
    (database-set-modified-p dbc-database t)
    (db-message "Record deleted")
    (db-next-record 1)))

(defun db-yank-record (endp)
  "Insert, and make current, the most recently deleted record.
The deleted record is inserted before the current record.
With prefix argument ENDP, insert at end of database and don't select it."
  (interactive "P")
  (unless (edb--1D dbc-database :deleted-record)
    (error "No deleted record to yank."))
  (db-in-data-display-buffer
    ;; This is inelegant in the extreme, but the interaction of
    ;; `dbc-set-index' and db-{previous-next}-record and
    ;; `database-add-record' mystifies me.  --karl@owl.hq.ileaf.com (Karl Berry)
    (database-add-record (edb--1D dbc-database :deleted-record)
                         dbc-database (unless endp
                                        (edb--S :index)))
    (if endp
        ;; We go back to the current record below.
        (db-next-record 1)
      (dbc-set-index (1+ (edb--S :index)))
      (db-previous-record 1)
      (database-set-modified-p dbc-database t)))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record yanked"))

(defun db-copy-record (&optional arg)
  "Insert a copy of the current record in the database immediately after it.
The second of the two records is made the current record.
With a prefix argument, inserts that many copies."
  (interactive "p")
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (let* ((vov (edb--1D dbc-database :vov))
           (rec (aref vov (1- (edb--S :index)))))
      (while (> arg 0)
        (database-add-record (copy-sequence rec) dbc-database (edb--S :index))
        (dbc-set-index (1+ (edb--S :index)))
        (decf arg))))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record copied"))

(defun db-output-record-to-db (db)
  "Copy (output) the current record to database DB.
DB must be read in and compatible with the current database."
  ;; Make a list of databases compatible with this one.
  (interactive
   (list
    (let ((last (edb--1D dbc-database :db-for-output))
          choices sel)
      (dolist (db (edb--1all-known-databases))
        (when (and (not (eq db dbc-database))
                   (databases-compatible db dbc-database))
          (push (cons (or (database-print-name db)
                          (edb--1D db :file))
                      db)
                choices)))
      (unless choices
        (error "No compatible databases are currently read in!"))
      (unless (and last (member last (mapcar 'car choices)))
        (edb--1D! dbc-database :db-for-output (setq last nil)))
      (setq sel (completing-read
                 "Output record to which database (? for choices): "
                 choices nil t last))
      (unless (string= "" sel)
        (edb--1D! dbc-database :db-for-output sel))
      (cdr (assoc sel choices)))))
  (when db
    (when (db-summary-buffer-p)
      (dbs-synch-format-with-summary))
    (with-current-buffer (if (db-summary-buffer-p)
                             (edb--S :data-display-buffer)
                           (current-buffer))
      (let ((rec (aref (edb--1D db :vov) (1- (edb--S :index)))))
        (db-in-data-display-buffer
          (database-add-record rec db))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting
;;;

(defun db-sort (&optional dont-confirm)
  "Sort the database.  With a prefix argument, don't confirm the sort order."
  (interactive "P")
  (let ((cur (current-buffer)))
    (db-in-data-display-buffer
      (dbf-process-current-record-maybe t)
      ;; Save current record for the sake of `dbf-finished-sorting'.
      (edb--S! :record/before-sorting (aref (edb--1D dbc-database :vov)
                                            (1- (edb--S :index))))
      (edb--S! :buffer/before-sorting cur)
      (if dont-confirm
          (progn
            (database-sort dbc-database)
            (dbf-finished-sorting))
        (database-sort-interface dbc-database)))))

(defun dbf-finished-sorting ()
  ;; Recompute the current record's index, dropping the saved ref immediately.
  (let ((rec (prog1 (or (edb--S :record/before-sorting)
                        (aref (edb--1D dbc-database :vov) 0))
               (edb--S! :record/before-sorting nil))))
    (dbc-set-index (catch t (db-lmap (lambda (record)
                                       (when (eq rec record)
                                         (throw t db-lmap-index)))
                                     dbc-database))
                   rec))

  (let (sumbuf window)
    (when (setq sumbuf (edb--S :sumbuf))
      ;; Force summary refresh (due to re-ordering only) and maybe display.
      (with-current-buffer sumbuf (edb--S! :nrecords -1))
      (if (setq window (get-buffer-window sumbuf))
          (with-selected-window window (db-summary))
        (save-window-excursion
          (with-current-buffer sumbuf (db-summary))))))

  ;; The index shown in the mode line is correct, but the database may have
  ;; been marked as modified, and that change hasn't made it to the mode line.
  (force-mode-line-update)

  (switch-to-buffer (prog1 (or (edb--S :buffer/before-sorting)
                               (current-buffer))
                      (edb--S! :buffer/before-sorting nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun db-field-query-replace ()
  "Replace some instances of a value in this field with some other value.
Confirms before each replacement."
  (interactive)
  (unless (edb--S :this-fidx)
    (error "Call this when on a field."))
  (let* ((iftxt    (edb--S :iftxt))
         (cur-idx  (edb--S :index))
         (dispspec (edb--S :this-ds))
         (fsno     (edb--S :this-fidx))
         (ridx     (edb--1ds-record-index dispspec))
         (fname    (db-fname<-fno ridx dbc-database))
         (ordfunc  (db-rs-ordfunc
                    (aref (edb--1D dbc-database :elaborated-rfspecs) ridx)))
         ov                             ; original value
         ov-printed
         rv                             ; replacement value
         rv-printed)
    (dbf-process-current-record-maybe nil)
    (setq ov (db-callconvert
              (edb--1ds-display->actual dispspec)
              (read-string "Query replace: ")
              ;; No previous value or record.
              nil nil
              ridx))
    (db-check-constraint ov nil ridx dbc-database)
    ;; Must keep in mind that this is not necessarily what the user typed.
    (setq ov-printed (db-callconvert
                      (edb--1ds-actual->display dispspec)
                      ov nil ridx))

    (setq rv (db-callconvert
              (edb--1ds-display->actual dispspec)
              (read-string (format "Query replace %s with: "
                                   ov-printed))
              nil nil ridx))
    (db-check-constraint rv nil ridx dbc-database)
    (setq rv-printed (db-callconvert
                      (edb--1ds-actual->display dispspec)
                      rv nil ridx))

    (db-maprecords
     (lambda (record)
       (when (= 0 (funcall ordfunc ov (aref record ridx)))
         (db-display-record record t)
         (db-skip-string-forward (aref iftxt 0))
         (edb--S! :this-fidx 0)
         (db-next-field-internal fsno)
         ;; fixme: handle strings too big for minibuffer. --ttn
         (when (y-or-n-p (format "(%s) Replace `%s' with `%s'? "
                                 fname ov-printed rv-printed))
           ;; It's a bit extreme that this errs if the value
           ;; fails to meet the constraint.
           (db-check-constraint rv record ridx dbc-database)
           (aset record ridx rv)))))
    (db-message "Replacement done")
    (db-jump-to-record cur-idx t)))

(defun db-accept-record ()
  "Install the current record in the database; make any changes permanent."
  (interactive)
  (dbf-process-current-record-maybe t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching
;;;

(defun db-search-field (pattern &optional mark)
  "Search for occurrences of PATTERN in the current field of any record.
Finds the first match after the current record; wraps around automatically.
With prefix argument, marks all matches in addition to going to the first one.
If hiding is in effect, hidden records are ignored."
  (interactive
   (list (let ((this-fidx (edb--S :this-fidx)))
           (unless this-fidx
             (error "Not on a field in a data display buffer."))
           (read-string (format "Search in %s for: "
                                (dbf-this-field-name (edb--S :this-ds)))
                        (aref (edb--S :search-defaults) this-fidx)))
         current-prefix-arg))
  (if (equal "" pattern)
      (error "You didn't enter a pattern for which to search."))

  (let* ((cur-rec (aref (edb--1D dbc-database :vov) (1- (edb--S :index))))
         (mtag (edb-tag :markedp dbc-database))
         (this-ds (edb--S :this-ds))
         (pat (db-parse-match-pattern pattern this-ds))
         (pdisp (db-print-match-pattern pat this-ds))
         (ridx (edb--1ds-record-index this-ds))
         (rs (aref (edb--1D dbc-database :elaborated-rfspecs) ridx))
         (this-field-index (edb--S :this-fidx))
         this-field
         (fname (dbf-this-field-name this-ds))
         ;; success is t if we've already found some match.
         ;; The idea is that we'll move to the record at success-index when
         ;; we're done with the search; if success is nil then we're looking
         ;; for such a record.  This is either because we haven't found one or
         ;; because we have only found one before :index in the database.
         success success-record success-index
         (matches 0))
    (aset (edb--S :search-defaults) this-field-index pdisp)
    (if mark
        (db-message "Marking all %s in %s..." pdisp fname)
      (db-message "Searching in %s for %s..." fname pdisp))
    (db-lmap
     (lambda (record)
       (when (or mark (not success))
         (setq this-field (aref record ridx))
         (when (db-match pat this-field rs)
           (if (not success)
               (setq success-record record
                     success-index db-lmap-index
                     success t))
           (when mark
             (incf matches)
             (edb-tagx mtag record))))
       ;; We're looking for a match in some record besides the displayed
       ;; one and, preferrably, after it.  This permits the first success
       ;; succeeding the current record to overwrite the first success
       ;; preceding the current record.  This means that searches can't
       ;; abort after a success, since that success might be before the
       ;; current record.
       (when (eq record cur-rec)
         (setq success nil)))
     dbc-database
     (edb--S :hide-p))
    (if success-index
        (if (= (edb--S :index) success-index)
            (db-message "This record has the only match for %s" pdisp)
          ;; This takes care of committing any changes to the current record.
          (dbf-goto-record-internal
            (dbc-set-index success-index success-record))
          (db-move-to-field-exact this-field-index)
          (if mark
              (progn (dbf-set-summary-out-of-date-p)
                     (db-message "Searching for %s...marked %s matches"
                                 pdisp matches))
            (db-message "Searching for %s...found" pdisp)))
      (db-message "Couldn't find a match in %s for %s" fname pdisp))))


(defun db-search ()
  "`db-search' is not yet implemented; use `db-search-field' instead.
In a future version of EDB, `db-search' will permit searching on all fields
of a record simultaneously."
  (interactive)
  (error "Unimplemented; use db-search-field instead (M-s from Edit mode)."))


;; fixme: convert to after-command hooks. --ttn

(defun db-isearch-forward ()
  "Like isearch-forward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-forward)
  (db-jump-to-point))

(defun db-isearch-backward ()
  "Like isearch-backward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-backward)
  (db-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding
;;;

(defun db-mark-record (&optional arg)
  "Toggle whether the current record is marked.
With a nonzero prefix argument, set it to be marked.
With a zero prefix argument, set it to be unmarked."
  (interactive "P")
  (db-in-data-display-buffer
    (let* ((mtag (edb-tag :markedp dbc-database))
           (idx (edb--S :index))
           (rec (aref (edb--1D dbc-database :vov) (1- idx))))
      (funcall (if (if arg
                       (not (zerop (prefix-numeric-value arg)))
                     (not (edb-tagp mtag rec)))
                   'edb-tagx
                 'edb-tag-)
               mtag rec)
      (dbf-update-summary-item idx)
      (dbc-set-index idx rec)
      (force-mode-line-update))))

(defun db-hide-record (&optional arg)
  "Change whether the current record is hidden.
With a nonzero prefix argument, set it to be hidden.
With a zero prefix argument, set it to be unhidden."
  (interactive "P")
  (db-in-data-display-buffer
    (let* ((htag (edb-tag :hiddenp dbc-database))
           (idx (edb--S :index))
           (rec (aref (edb--1D dbc-database :vov) (1- idx))))
      (funcall (if (if arg
                       (not (zerop (prefix-numeric-value arg)))
                     (not (edb-tagp htag rec)))
                   'edb-tagx
                 'edb-tag-)
               htag rec)
      (if (edb--S :hide-p)
          (dbf-update-summary-item idx)
        ;; Automatically turn on the effect of hiding.
        (dbc-set-hide-p t)
        ;; Update all marks, since potentially all have to be displayed now.
        (dbf-update-summary-marks))
      (dbc-set-index idx rec)
      (force-mode-line-update))))

(defun db-mark-searched-records ()
  "Mark all records found in search STRING of FIELD."
  (interactive)
  (setq current-prefix-arg "1")
  (call-interactively 'db-search-field "1")
  (let ((sumbuf (edb--S :sumbuf)))
    (when sumbuf
      (save-window-excursion
        (switch-to-buffer-other-window sumbuf)
        (dbs-synch-format-with-summary)))))


(defun db-hide-unmarked-records ()
  "Hide all unmarked records.  Also, clear mark bits and enable hiding."
  (interactive)
  (db-in-data-display-buffer
    (let ((mtag (edb-tag :markedp dbc-database))
          (htag (edb-tag :hiddenp dbc-database)))
      (db-lmap
       (lambda (record)
         (if (edb-tagp mtag record)
             (edb-tag- mtag record)
           (edb-tagx htag record)))
       dbc-database
       t))
    (dbc-set-hide-p t)
    (dbf-update-summary-marks)
    ;; fixme: redisplay? --ttn
    ))

(defun db-mark-unhidden-records ()
  "Mark all unhidden records.  Also clears all hide bits."
  (interactive)
  (db-in-data-display-buffer
    (let ((hiddenp (edb-tag :hiddenp dbc-database))
          (markedp (edb-tag :markedp dbc-database)))
      (db-lmap
       (lambda (record)
         (if (edb-tagp hiddenp record)
             (edb-tag- hiddenp record)
           (edb-tagx markedp record)))
       dbc-database))
    (dbc-set-hide-p t)
    (dbf-update-summary-marks)
    ;; fixme: redisplay? --ttn
    ))

(defun db-unhide-all ()
  "Clear the hide bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (let ((htag (edb-tag :hiddenp dbc-database)))
      (db-lmap
       (lambda (record)
         (edb-tag- htag record))
       dbc-database))
    (dbc-set-index (edb--S :index))
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-unmark-all ()
  "Clear the mark bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (let ((mtag (edb-tag :markedp dbc-database)))
      (db-lmap
       (lambda (record)
         (edb-tag- mtag record))
       dbc-database))
    (dbc-set-index (edb--S :index))
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-hiding-toggle (&optional arg)
  "Change whether hiding is in effect.
With a nonzero prefix argument, turn hiding on.
With a zero prefix argument, turn hiding off.

This does not change the current hide-function, and a hide bit is always
computed for each record, but hide bits have no effect on any operations
if hiding is not in effect."
  (interactive "P")
  (db-in-data-display-buffer
    (dbc-set-hide-p (if arg
                        (not (zerop (prefix-numeric-value arg)))
                      (not (edb--S :hide-p))))
    ;; Refill summary buffer whenever displayed set of records changes,
    ;; including when switching to no hiding and showing hidden records.
    (cond
     ((edb--S :invisible-hidden-p)
      ;; If the hidden records weren't being shown, the records that
      ;; should be displayed in the summary buffer just changed.
      (dbf-fill-summary-buffer t))
     (t
      (dbf-update-summary-marks)))
    (force-mode-line-update)
    (db-message "Hiding is now %sin effect" (if (edb--S :hide-p) "" "not "))))

(defun db-hiding-set ()
  "Set the criteria for automatically determining whether to hide a record.
This isn't implemented yet."
  (interactive)
  (error "db-hiding-set is not yet implemented."))

(defun db-toggle-show-hidden-records (&optional arg)
  "Toggle whether hidden records are shown in the summary.
With a nonzero prefix argument, show hidden records in the summary.
With a zero prefix argument, don't show hidden records in the summary."
  (interactive "P")
  (db-in-data-display-buffer
    (let ((inv-p (if arg
                     (zerop (prefix-numeric-value arg))
                   (not (edb--S :invisible-hidden-p))))
          (sumbuf (edb--S :sumbuf)))
      (edb--S! :invisible-hidden-p inv-p)
      (if (not inv-p)
          ;; If we weren't showing hidden records, we might as well start from
          ;; scratch in filling the summary buffer.
          (dbf-fill-summary-buffer t)
        (when sumbuf
          (db-in-buffer sumbuf
            (let ((buffer-read-only nil))
              (goto-char (point-min))
              (delete-matching-lines "^.\\[")
              (dbs-move-to-proper-record)))))
      (db-message "Hidden records will %snow be shown"
                  (if inv-p "not " "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reporting
;;;

(defun db-report (filename &optional markedp)
  "Create a report according to format specifications in FILENAME.
Prefix argument MARKEDP, if non-nil, means report on only marked records.
If hiding is in effect, hidden records are not reported upon.
When called interactively, prompt for FILENAME."
  (interactive
   (list (let* ((ddb-spec (edb--S :ddb-spec))
                (conn (when ddb-spec
                        (with-current-buffer ddb-spec
                          (get-text-property (point-min) :connection))))
                (tspec (when conn
                         (edb--with-callable-connection conn
                           (plist-get (conn :schema) :report)))))
           (or tspec (read-file-name "Report format file: " nil nil t)))
         current-prefix-arg))
  (dbf-process-current-record-maybe t)
  (let* ((db dbc-database)
         (lasfl (db-format->lines/sforms
                 (cond ((consp filename)
                        (plist-get filename :text))
                       ((stringp filename)
                        (with-temp-buffer
                          (insert-file-contents filename)
                          (buffer-string))))
                 db nil nil t))
         (rfunc `(lambda (formatted-record)
                   (insert ,@(cdr lasfl))))
         (hide-p (edb--S :hide-p))
         (mtag (edb-tag :markedp db)))
    (switch-to-buffer (get-buffer-create "*Database Report*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (db-lmap
     (lambda (record)
       (when (or (not markedp) (edb-tagp mtag record))
         (funcall rfunc record)))
     db
     hide-p)
    (goto-char (point-min))))

;;; db-interfa.el ends here
;;; db-types.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Library of types for EDB database fields.

;; This file contains predefined types.  For efficiency, they're defined
;; in terms of the displayspec abstraction instead of via format strings.
;; Improvements and additions are welcome.  Predefined types for dates and
;; times can be found in db-time.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contents
;;;

;;; Contents
;;; Variables
;;; Displaytype
;;; Enumeration Displaytypes
;;; Numbers
;;; Booleans
;;; Strings


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar db-enum-ignore-case t
  "If non-nil, assume that any association lists created from
enumeration types input names are to include both upper and lower case
versions of the name, if distinct.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaytype
;;;

(defalias 'define-displaytype-from-displayspec 'edb-define-displaytype)
(put 'edb-define-displaytype 'lisp-indent-hook 2)
(defun edb-define-displaytype (name source &rest override)
  "Define a displaytype NAME (a symbol) with SOURCE.
SOURCE may be a displayspec, the name (a symbol) of a currently
defined type, or nil.  In all cases, a new displayspec object
is created and then modified by OVERRIDE, a sequence of alternating
keywords and values, and finally added to a global list.  Return NAME."
  (unless (symbolp name)
    (error "Not a symbol: %s" name))
  (let ((ds (cond ((edb--1ds-p source) (edb--copy-1ds source))
                  ((not source) (apply 'edb--make-1ds override))
                  ((and (symbolp source)
                        (let ((ds (db-dspec<-dtype source)))
                          (when ds (edb--copy-1ds ds)))))
                  (t (error "Not a displayspec or known displaytype: %s"
                            source)))))
    (when override
      (let ((ls override) (kwidx (get 'edb--1ds 'kwidx)))
        (while ls
          (aset ds (or (cdr (assq (car ls) kwidx))
                       (error "No such keyword: %s" (car ls)))
                (cadr ls))
          (setq ls (cddr ls)))))
    (puthash name ds (edb--G :1displaytypes))
    name))

(defalias 'define-recordfieldtype-from-recordfieldspec 'edb-define-recordfieldtype)
(put 'edb-define-recordfieldtype 'lisp-indent-hook 2)
(defun edb-define-recordfieldtype (name source &rest override)
  "Define a recordfieldtype NAME (a symbol) with SOURCE.
SOURCE may be a recordfieldspec, the name (a symbol) of a currently
defined type, or nil.  In all cases, a new recordfieldspec object
is created and then modified by OVERRIDE, a sequence of alternating
keywords and values, and finally added to a global list.  Return NAME."
  (unless (symbolp name)
    (error "Not a symbol: %s" name))
  (let ((rs (cond ((edb--v1-rs-p source) (edb--copy-v1-rs source))
                  ((not source) (apply 'edb--make-v1-rs override))
                  ((and (symbolp source)
                        (let ((rs (db-rfspec<-rftype source)))
                          (when rs (edb--copy-v1-rs rs)))))
                  (t (error "Unknown recordfieldspec or recordfieldtype: %s"
                            source)))))
    (when override
      (let ((ls override) (kwidx (get 'edb--v1-rs 'kwidx)))
        (while ls
          (aset rs (or (cdr (assq (car ls) kwidx))
                       (error "No such keyword: %s" (car ls)))
                (cadr ls))
          (setq ls (cddr ls)))))
    (puthash name rs (edb--G :1recordfieldtypes))
    name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enumeration displaytypes
;;;

;; An enumeration displaytype is used for fields whose values are one of a
;; fixed set of alternatives.

;; For each alternative, four pieces of information are specified
;; (see documentation for `define-enum-type' for details).  We have the
;; following options for storing the relationships between those pieces of
;; information, which must be used by such functions as the displayspec's
;; display->actual function:
;;  * in global variables keyed by enum-type name and looked up when needed,
;;  * in database-local variables similar to the above, or
;;  * in the functions themselves (the functions are created when the
;;    enum-type is defined and must in any event contain some specific
;;    information such as the enum-type name).

;; Where to Keep Info: If any of the information is kept in database-local
;;    variables, all of it should be, because database-local variables are
;;    saved when the database is stored in internal file layout and restored
;;    when it is read in again.  However, other information is kept in global
;;    variables such as :1displaytypes, so it is problematic to keep any
;;    information in database-local variables; it is especially bad to have
;;    the type half-defined so that an error is produced if the type is
;;    redefined but the existing information is insufficient to provide the
;;    full enum type functionality.  If all the information is stored in
;;    database-local variables, then it is unnecessary for databases stored in
;;    internal file layout to have calls to define-enum-type in their
;;    auxiliary files.


(defalias 'define-enum-type 'edb-define-enumtype)
(defun edb-define-enumtype (typename alternatives &optional optstring)
  "Make TYPENAME (a symbol or string) an enumerated type.
Both a displaytype and a recordfieldtype are created.

ALTERNATIVES is a list.  Each alternative is a list of up to four components:
 the internal representation, any constant Lisp object, often a string;
 the input representation typed by the user to specify this alternative,
   a string or list of strings (for multiple input representations);
 the display representation, a string; and
 the file storage representation, a string.

If the input representation is omitted and the internal representation is a
string, that string is used.  If the display representation is omitted, it
defaults to the first input representation.  The display representation is
automatically also a valid input representation.  If the file storage
representation is omitted, it defaults to the display representation.
If all the other components are omitted, the internal representation string
may be used in place of a one-element list containing just it.

Optional argument OPTSTRING is a displayspec option string."

  (let ((type (if (stringp typename)
                  (intern typename)
                typename))
        d->a dinput->a a->d a->s a-s-differ
        internal input display storage)
    (dolist (alt alternatives)
      (unless (listp alt)
        (setq alt (list alt)))
      (setq internal (car alt)
            input (or (car (cdr alt)) internal)
            input (if (listp input) input (list input))
            display (or (car (cdr (cdr alt))) (car input))
            storage (or (nth 3 alt) display)

            d->a (nconc d->a (mapcar (lambda (irep)
                                       (cons irep internal))
                                     input))
            a-s-differ (or a-s-differ (not (equal internal storage))))
      (unless (member display input)
        (push (cons display internal) dinput->a))
      (push (cons internal display) a->d)
      (push (cons internal storage) a->s))
    ;; The order is significant in db-enum-make-help-info.
    ;; [It's not clear whether that's a feature or a bug.]
    (setq d->a (nconc d->a (nreverse dinput->a))
          a->d (nreverse a->d)
          a->s (and a-s-differ (nreverse a->s)))

    (flet ((enum-do-completions
            (rep type d->a)
            ;; Given a string REP and an enum TYPE, return REP if it is a
            ;; valid input representation, otherwise see if it completes to a
            ;; valid one, and show the possible completions.
            (let ((hit (assoc rep d->a)))
              (if hit
                  (cdr hit)
                (let* ((completion-ignore-case db-enum-ignore-case)
                       (try (try-completion rep d->a)))
                  (if (and try (setq hit (assoc try d->a)))
                      (cdr hit)
                    (dbf-set-this-field-text (or try ""))
                    (setq rep
                          (completing-read
                           (format "Enter \"%s\" enum value (? for list): "
                                   type)
                           d->a nil t (or try "")))
                    (cdr (assoc rep d->a)))))))
           (make-enum-member-orderer
            (type a->d v< v= v>)
            ;; Given an enum type TYPE, create an ordering function which
            ;; compares two items in the enum type's alternatives list.  The
            ;; resulting function returns V< if its first argument precedes
            ;; its second argument in the alternative list, V= if they're
            ;; equal or neither appears, and V> if the first follows the
            ;; second.
            ;;
            ;; If one of the arguments doesn't appear in the alternatives
            ;; list, the other is considered to precede it.
            `(lambda (o1 o2)
               (if (equal o1 o2)
                   ,v=
                 (let ((ls ',a->d)
                       (rv ,v=)
                       alt-o)
                   (while ls
                     (setq alt-o (car ls))
                     (cond ((equal (car alt-o) o1)
                            (setq rv ,v<
                                  ls nil))
                           ((equal (car alt-o) o2)
                            (setq rv ,v>
                                  ls nil))
                           (t
                            (setq ls (cdr ls)))))
                   rv)))))

      (edb-define-displaytype type
        (db-dspec<-type/opts nil (when optstring
                                   (split-string optstring ","))
                             t)
        :indent            nil
        :display->actual `(lambda (input-rep)
                            (,(symbol-function 'enum-do-completions)
                             input-rep ',type ',d->a))
        :actual->display `(lambda (enum-val)
                            (cdr (assoc enum-val ',a->d))))

      (edb-define-recordfieldtype type nil
        :type           type
        :default-value  ""
        :order-fn       (make-enum-member-orderer type a->d -1 0 1)
        :sort-fn        (make-enum-member-orderer type a->d t nil nil)
        :match-function 'db-string-match-function
        :help-info      (with-temp-buffer
                          (let ((standard-output (current-buffer)))
                            (princ (format "%s:  an enumerated type.  " type))
                            (display-completion-list (mapcar 'car d->a))
                            (buffer-string)))
        :actual->stored (when a->s
                          `(lambda (enum-val)
                             (cdr (assoc enum-val ',a->s))))
        :stored->actual (when a->s
                          `(lambda (stored-val)
                             (car (rassoc stored-val ',a->s))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;


;;;
;;; Integers
;;;

(edb-define-displaytype 'integer nil
  :indent           nil
  :actual->display 'int-to-string
  :display->actual 'string-to-int)

(edb-define-recordfieldtype 'integer nil
  :type           'integer
  :default-value   0
  :actual->stored 'int-to-string
  :stored->actual 'string-to-int
  :order-fn       'db-number-order
  :sort-fn        '<
  :match-function '=
  :help-info      "An integer.")

(edb-define-displaytype 'integer-or-nil nil
  :indent           nil
  :actual->display 'db-number-or-nil->string
  :display->actual 'db-string->integer-or-nil)

(edb-define-recordfieldtype 'integer-or-nil nil
  :type           'integer-or-nil
  :default-value   0
  :actual->stored 'db-number-or-nil->string
  :stored->actual 'db-string->integer-or-nil
  :order-fn       'db-number-or-nil-order-nil-greatest
  ;; :sort-fn     '<
  :match-function 'equal
  :help-info      "An integer, or nil.")



;;;
;;; Numbers (a number is an integer or a float)
;;;

(let ((correct-string-to-number
       ;; Emacs 19.28 bug: isfloat_string fails when arg has trailing spaces,
       ;; so (s2n "5.4") => 5.4 but (s2n "5.4 ") => 5 .
       (if (= 5 (string-to-number "5.4 "))
           (lambda (string)
             (if (string-match " " string)
                 ;; chop everything after the first space
                 (string-to-number (substring string 0 (match-beginning 0)))
               (string-to-number string)))
         'string-to-number)))

  (edb-define-displaytype 'number nil
    :indent           nil
    :actual->display 'number-to-string
    :display->actual  correct-string-to-number)

  (edb-define-recordfieldtype 'number nil
    :type           'number
    :default-value   0
    :actual->stored 'number-to-string
    :stored->actual  correct-string-to-number
    :order-fn       'db-number-order
    :sort-fn        '<
    :match-function '=
    :help-info      "A number."))

(edb-define-displaytype 'number-or-nil nil
  :indent           nil
  :actual->display 'db-number-or-nil->string
  :display->actual 'db-string->number-or-nil)

(edb-define-recordfieldtype 'number-or-nil nil
  :type           'number-or-nil
  :default-value   0
  :actual->stored 'db-number-or-nil->string
  :stored->actual 'db-string->number-or-nil
  :order-fn       'db-number-or-nil-order-nil-greatest
  ;; :sort-fn     '<
  :match-function 'equal
  :help-info      "A number, or nil.")


;;;
;;; Sorting and ordering
;;;

(defsubst db-number-order (a b)
  "Return -1, 0, or 1 depending on whether A < B, A = B, or A > B."
  (cond ((= a b) 0)
        ((< a b) -1)
        (t 1)))

(defun db-number-or-nil-order-nil-greatest (a b)
  "Like db-number-order, but nil is treated as greater than any integer.
This puts nil after integers in an increasing list."
  (cond ((and a b)
         (db-number-order a b))
        (a -1)
        (b 1)
        (t 0)))

(defun db-number-or-nil-order-nil-least (a b)
  "Like db-number-order, but nil is treated as smaller than any integer.
This puts nil after integers in a decreasing list."
  (cond ((and a b)
         (db-number-order a b))
        (a 1)
        (b -1)
        (t 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Booleans
;;;

(edb-define-displaytype 'yes-no nil
  :indent           nil
  :min-width        3
  :max-width        3
  :actual->display 'db-boolean->yes-no-string
  :display->actual 'db-yes-no-string->boolean)

(edb-define-recordfieldtype 'boolean nil
  :type           'boolean
  :default-value   nil
  :actual->stored 'db-boolean->yn-string
  :stored->actual 'db-yn-string->boolean
  :order-fn       'db-boolean-order-function
  :sort-fn        'db-boolean-lessp
  :match-function 'eq
  :help-info      "A boolean value.")

(defsubst db-boolean->yes-no-string (b)
  (if b "Yes" "No"))

(defun db-yes-no-string->boolean (s)
  (let ((low (downcase s)))
    (cond ((string= "yes" low)
           t)
          ((string= "no" low)
           nil)
          (t
           (error "`%s' is not `Yes' or `No'." s)))))

(defsubst db-boolean->yn-string (b)
  (if b "Y" "N"))

(defsubst db-yn-string->boolean (s)
  (string= "y" (downcase s)))

(defun db-boolean-order-function (b1 b2)
  ;; NOTE: t < nil so that in the "increasing" ordering "true" things occur
  ;;       before false ones.  (This is somewhat arbitrary.)
  (cond ((or (and b1 b2)
             (not (or b1 b2)))
         0)
        (b1
         -1)
        (t
         1)))

(defsubst db-boolean-lessp (b1 b2)
  (and (not b1) b2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings and variants
;;;

;; Type: string

(edb-define-displaytype 'string nil
  :max-height             nil
  :indent                 t
  :match-actual->display 'db-string-match-actual->display
  :match-display->actual 'db-string-match-display->actual)

(edb-define-recordfieldtype 'string nil
  :type           'string
  :default-value  ""
  :order-fn       'db-string-order-ci
  :sort-fn        'db-string-lessp-ci
  :match-function 'db-string-match-function
  :help-info      "A string.")

;; Type: one-line-string

(edb-define-displaytype 'one-line-string 'string
  :min-height 1
  :max-height 1
  :indent     nil)

(edb-define-recordfieldtype 'one-line-string 'string
  :type 'one-line-string)

;; Type: string-or-nil

(edb-define-displaytype 'string-or-nil 'string
  :actual->display 'db-string-or-nil->string)

(edb-define-recordfieldtype 'string-or-nil 'string
  :type           'string-or-nil
  :order-fn       'db-string-or-nil-order-ci
  :sort-fn        'db-string-or-nil-lessp-ci
  :match-function 'db-string-or-nil-match-function)

;; Type: nil-or-string

(edb-define-displaytype 'nil-or-string 'string-or-nil
  :display->actual 'db-string->nil-or-string)

(edb-define-recordfieldtype 'nil-or-string 'string-or-nil
  :type 'nil-or-string)

;; Type: one-line-string-or-nil

(edb-define-displaytype 'one-line-string-or-nil 'one-line-string
  :actual->display 'db-string-or-nil->string)

(edb-define-recordfieldtype 'one-line-string-or-nil
    'one-line-string
  :type           'one-line-string-or-nil
  :order-fn       'db-string-or-nil-order-ci
  :sort-fn        'db-string-or-nil-lessp-ci
  :match-function 'db-string-or-nil-match-function)


;; Helping functions for type string

(defsubst db-string-lessp-ci (s1 s2)
  "Case-insensitive version of string-lessp."
  (let ((v (compare-strings s1 0 nil s2 0 nil t)))
    (when (numberp v)
      (> 0 v))))

(defun db-string-order-ci (s1 s2)
  "Return -1, 0, or 1 depending on whether string S1 is lexicographically
less than, equal to, or greater than S2.  Case-insensitive."
  (let ((v (compare-strings s1 0 nil s2 0 nil t)))
    (cond ((eq t v) 0)
          ((> 0 v) -1)
          (t        1))))

;;; Matching strings

;; Pattern is a list of 'regexp and a regexp or a list of 'string, a
;; regexp, and a string.
(defun db-make-regexp-pattern (rx)
  (list 'regexp rx))
(defun db-regexp-pattern-regexp (pat)
  (car (cdr pat)))

(defun db-make-string-pattern (s &optional rx)
  (list 'string (or rx (regexp-quote s)) s))
(defun db-string-pattern-regexp (pat)
  (car (cdr pat)))
(defun db-string-pattern-string (pat)
  (car (cdr (cdr pat))))

(defsubst db-string-match-function (pat s)
  ;; The second element of pat is a regexp whether the pat is a
  ;; string or a regexp.
  (string-match (if (stringp pat)       ; slack: handle string
                    pat
                  (car (cdr pat)))
                s))

(defun db-string-match-display->actual (s)
  ;; Return a pattern.
  (if (string-match
       "^[ \t]*\\(/\\|regexp[ \t]+\\)" ;;; was: dbm-string-regexp-prefix
       s)
      (db-make-regexp-pattern (substring s (match-end 0)))
    (db-make-string-pattern s)))

(defun db-string-match-actual->display (pat)
  (cond ((eq (car pat) 'string)
         (db-string-pattern-string pat))
        ((eq (car pat) 'regexp)
         (concat
          "/" ;;; was: dbm-string-regexp-prefix-string
          (db-regexp-pattern-regexp pat)))
        (t
         (error "db-string-match-actual->display: bad pat %s" pat))))

;; Helping functions for type string-or-nil

(defsubst db-string-or-nil->string (s-o-n)
  (or s-o-n ""))

(defsubst db-string-or-nil-lessp-ci (x y)
  (db-string-lessp-ci (db-string-or-nil->string x)
                      (db-string-or-nil->string y)))

(defsubst db-string-or-nil-order-ci (x y)
  (db-string-order-ci (or x "") (or y "")))

(defsubst db-string-or-nil-match-function (pat s-o-n)
  (string-match (if (stringp pat)
                    pat
                  (if pat (car (cdr pat)) ""))
                (or s-o-n "")))

;; Helping functions for type nil-or-string

(defsubst db-string->nil-or-string (s)
  (if (equal "" s)
      nil
    s))

;;; db-types.el ends here
;;; db-summary.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Patterned in part after rmail-new-summary.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;


;;;
;;; Hooks
;;;

(defvar database-summary-mode-hooks nil
  "Normal hook run when switching to Database Summary mode.")


;;;
;;; Summary variables
;;;

(defun dbs-set-index (index)
  (edb--S! :index index)
  (edb--S! :index-fraction (format "%d/%d" index (edb--S :nrecords))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros for working in the correct buffer
;;;

(defmacro db-in-data-display-buffer (&rest body)
  (declare (indent 0) (debug body))
  `(db-in-buffer (or (edb--S :data-display-buffer)
                     (current-buffer))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating the summary
;;;


(defun db-summary ()
  "Display a summary (or directory) of records in a separate buffer.
When called from the summary buffer, this updates the summary.
The displayed format can be set with `dbf-set-summary-format'."
  (interactive)
  (db-in-data-display-buffer
    (cond ((= 0 (edb--1D dbc-database :nrecords))
           (delete-windows-on (edb--S :sumbuf))
           (db-message "Database is empty"))
          (t
           (let ((ddb (current-buffer))
                 (sumbuf (edb--S :sumbuf)))
             (unless (edb--S :sumfun)
               (dbf-set-summary-format (edb--S :sumfmt)))
             (unless sumbuf
               (edb--S! :sumbuf
                        (with-current-buffer
                            (generate-new-buffer
                             (format "%s-summary" (buffer-name ddb)))
                          (set (make-local-variable 'edb--bprops)
                               (edb--rinit '(:edb1 :bprops) (current-buffer)
                                           :size 5 :weakness 'key))
                          (set (make-local-variable 'dbc-database)
                               (buffer-local-value 'dbc-database ddb))
                          (edb--S! :data-display-buffer ddb)
                          (database-summary-mode)
                          (edb--S! :summaries (make-hash-table :weakness 'key))
                          (setq sumbuf (current-buffer)))))
             (when (with-current-buffer sumbuf
                     (dbs-out-of-date-p))
               (dbf-fill-summary-buffer))
             (pop-to-buffer sumbuf)
             (edb--S! :data-display-buffer ddb)
             ;; Go to proper line.
             (dbs-move-to-proper-record))))))


;; This is spelled out instead of being db-summary-mode because it's a
;; "standalone" major mode, while db-edit-mode and db-view-mode are
;; "cooperating" major modes (unimportant distinction, really --ttn).
(defun database-summary-mode ()
  "Summary buffer for database mode.
Most keystrokes perform the same function they do in the data display buffer.

Key bindings:

\\{database-summary-mode-map}"

  (setq major-mode 'database-summary-mode)
  (setq mode-name "Database Summary")

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)

  (use-local-map database-summary-mode-map)

  (setq mode-line-format
        (list
         "----- "
         (format "%-17s" (buffer-name (edb--S :data-display-buffer)))
         "   %[(" 'mode-name
         'minor-mode-alist
         '(:eval (concat (and (edb--S :hide-p) " Hide")
                         " "
                         (edb--S :index-fraction)))
         ")%]---"
         '(-3 . "%p")
         "-%-"))

  (edb--1run-hooks 'database-summary-mode-hooks)

  ;; Force an update.
  (edb--S! :nrecords -1))


(defsubst db-summary-buffer-p ()
  "T if this buffer is a database summary buffer."
  (eq major-mode 'database-summary-mode))


(defun db-summary-subset ()
  "Make EDB summary-listing based on hits of STRING search, of FIELD."
  (interactive)
  (db-mark-searched-records)
  (db-hide-unmarked-records)
  (db-toggle-show-hidden-records 0)
  (db-summary)
  (db-hiding-toggle 0)
  (db-hiding-toggle 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filling the summary
;;;

(defun dbs-insert-record-summary (record mhp mtag htag)
  ;; mhp -- mark-hidden-records-p
  (let ((one (if (edb-tagp mtag record) "+" " "))
        (two (if (and mhp (edb-tagp htag record)) "[" " "))
        (rest (substring (gethash record (edb--S :summaries)) 2)))
    (insert one two rest)))


(defun dbf-fill-summary-buffer (&optional movep)
  ;; fixme: check database nonempty. --ttn
  (let ((sum (edb--S :sumfun))
        (lines (edb--1D dbc-database :sum1lines))
        (hide (and (edb--S :invisible-hidden-p)
                   (edb--S :hide-p)))
        (mhp (edb--S :hide-p))
        (sumbuf (edb--S :sumbuf)))
    (when sumbuf
      (with-current-buffer sumbuf
        (let ((ht (edb--S :summaries))
              (mtag (edb-tag :markedp dbc-database))
              (htag (edb-tag :hiddenp dbc-database))
              buffer-read-only)
          (erase-buffer)
          (db-message "Computing summary...")
          (db-lmap
           (lambda (record)
             (unless (gethash record ht)
               (puthash record (funcall sum record) ht))
             (dbs-insert-record-summary record mhp mtag htag))
           dbc-database hide "Computing summary...%d")
          (db-message "Computing summary...done")
          ;; get rid of last newline.
          (backward-delete-char 1)
          (set-buffer-modified-p nil)
          (edb--S! :nrecords (edb--1D dbc-database :nrecords))
          (edb--S! :recompute-p nil)
          (edb--S! :index 0))
        (when movep
          (dbs-move-to-proper-record))))))

(defun dbf-update-summary-marks ()
  ;; Update just the marked and hidden summary markings efficiently.
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (let ((opoint (point))
            (hidep (not (or (not (edb--S :invisible-hidden-p))
                            (not (edb--S :hide-p)))))
            (mtag (edb-tag :markedp dbc-database))
            (htag (edb-tag :hiddenp dbc-database))
            line hiddenp)
        (unwind-protect
            (let ((sum1lines (edb--1D dbc-database :sum1lines))
                  buffer-read-only)
              (goto-char (point-min))
              (db-lmap
               (lambda (record)
                 (delete-char 1)
                 (insert (if (edb-tagp mtag record) "+" " "))
                 (backward-char 1)
                 (setq line 0
                       hiddenp (and (edb--S :hide-p) (edb-tagp htag record)))
                 ;; Each summary item spans exactly sum1lines screen lines.
                 (while (< line sum1lines)
                   (forward-char 1)
                   (delete-char 1)
                   (insert (if hiddenp "[" " "))
                   (forward-line 1)
                   (incf line)))
               dbc-database
               hidep))
          (goto-char opoint))))))


(defun dbf-update-summary-item (index)
  ;; Update just changes to one record in the summary efficiently.
  (let ((record (aref (edb--1D dbc-database :vov) (1- index)))
        (mtag (edb-tag :markedp dbc-database))
        (htag (edb-tag :hiddenp dbc-database))
        (sum (edb--S :sumfun))
        (sumbuf (edb--S :sumbuf)))
    (when (and sumbuf (or (not (edb-tagp htag record))
                          (not (edb--S :invisible-hidden-p))
                          (not (edb--S :hide-p))))
      (db-in-buffer sumbuf
        (let ((orig (edb--S :index)))
          (unwind-protect
              (let ((ht (edb--S :summaries))
                    buffer-read-only)
                (puthash record (funcall sum record) ht)
                (dbs-move-to-proper-record index)
                ;; assuming at beginning of line
                (delete-region (point)
                               (progn
                                 (forward-line
                                  (edb--1D dbc-database :sum1lines))
                                 (point)))
                (dbs-insert-record-summary record (edb--S :hide-p) mtag htag))
            ;; save old line and column instead.
            (dbs-move-to-proper-record orig)))))))


(defun db-format->lines/sforms (format db indent nlp vheightp)
  ;; Take a format and return a cons of two values: a number and a list.
  ;; The list is list of forms which, when evaluated with variable
  ;; `formatted-record' bound, evaluate to strings; these can be used as
  ;; argumentes to concat, insert, etc.  The number is the number of lines
  ;; occupied by the items when inserted.
  ;;
  ;; Signal an error if any displayspec has nonequal min-height and
  ;; max-height, unless VHEIGHTP is non-nil, in which case the number
  ;; returned is a minimum.  NLP non-nil means add a newline.
  (let ((ph (and (string-match "\\\\\\\\" format) ; backslash place holder
                 (db-unused-char-in-string format)))
        (lines 0)
        results beg end ds minh maxh)
    (when (and indent (> indent 0))
      (setq format (concat (make-string indent 32)
                           (replace-regexp-in-string
                            "\n"
                            (concat "\n" (make-string indent 32))
                            format))))
    (when ph
      (setq format (replace-regexp-in-string
                    (regexp-quote "\\\\")
                    (char-to-string ph)
                    format)))

    (while (string-match db-ds+opts-rx format)
      (setq beg (match-beginning 0)
            end (match-end 0)
            ds (db-dspec<-string format db)
            minh (edb--1ds-min-height ds)
            maxh (edb--1ds-max-height ds))
      ;; fixme: should not be necessary. --ttn
      (unless minh
        (setf minh (setf (edb--1ds-min-height ds) 1)))
      (unless (or vheightp maxh)
        (setf maxh (setf (edb--1ds-max-height ds) minh)))
      (if (or vheightp (= minh maxh))
          (incf lines (1- minh))
        (error "Min- (%s) and max (%s) heights differ in summary displayspec."
               minh maxh))
      (unless (zerop beg)
        (let ((literal (substring format 0 beg)))
          (when ph
            (subst-char-in-string ph ?\\ literal t))
          (push literal results)
          (incf lines (db-count-newlines literal))))
      (push `(db-ds-printed ,ds formatted-record) results)
      (setq format (substring format end)))
    (when nlp (setq format (concat format "\n")))
    (unless (equal "" format)
      (when ph
        (subst-char-in-string ph ?\\ format t))
      (push format results))
    (incf lines (db-count-newlines format))
    (cons lines (nreverse results))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Synching the format and summary buffers
;;;

(defsubst dbs-in-synch-p ()
  (= (with-current-buffer (edb--S :data-display-buffer)
       (edb--S :index))
     (edb--S :index)))

(defsubst dbs-out-of-date-p ()
  (or (edb--S :recompute-p)
      (not (= (edb--1D dbc-database :nrecords)
              (edb--S :nrecords)))))

(defsubst dbf-set-summary-out-of-date-p ()
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (edb--S! :recompute-p t))))

(defun dbs-synch-format-with-summary ()
  ;; Ensure that the data display and summary buffers
  ;; have the same current record.
  (cond ((dbs-out-of-date-p) (dbs-synch-summary-with-format))
        ((dbs-in-synch-p))
        (t (let ((sum-index (edb--S :index)))
             (db-in-buffer (edb--S :data-display-buffer)
               (db-select-record sum-index))))))

(defun dbs-synch-summary-with-format ()
  (when (dbs-out-of-date-p)
    (db-in-buffer (edb--S :data-display-buffer)
      (dbf-fill-summary-buffer)))
  ;; If we just did the above, it will clearly be out of synch.
  ;; But it might be even if it wasn't out of date.
  (unless (dbs-in-synch-p)
    (dbs-move-to-proper-record)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving about
;;;

(defsubst dbs-goto-nth-summary (n)
  ;; NOTE: If hidden records aren't shown in the summary (and that should be
  ;;       an option), then this is wrong.  And in that case it's better to do
  ;;       relative than absolute motion.
  (goto-line (1+ (* (edb--1D dbc-database :sum1lines) (1- n))))
  (edb--S! :point (point)))

(defun dbs-move-to-proper-record (&optional index)
  "Move point to the summary of the record shown in the format or to INDEX."
  ;; Make no assumptions about the current index.
  (if (with-current-buffer (edb--S :data-display-buffer)
        (or (not (edb--S :hide-p))
            (not (edb--S :invisible-hidden-p))))
      (let ((index (or index (with-current-buffer
                                 (edb--S :data-display-buffer)
                               (edb--S :index)))))
        (dbs-goto-nth-summary index)
        (dbs-set-index index))
    (let ((prev 0)
          (last nil)
          (pidx (or index               ; proper index
                    (with-current-buffer (edb--S :data-display-buffer)
                      (edb--S :index)))))
      (db-lmap
       (lambda (ignored-record)
         (if (<= db-lmap-index pidx)
             (setq prev (1+ prev)
                   last db-lmap-index)
           ;; Past it but still haven't found a nonhidden record.
           (unless last
             (setq prev 1
                   last db-lmap-index))))
       dbc-database
       t)
      ;; If there are no displayed records at all, this will fail.
      (unless (= last pidx)
        (db-message "Record %s does not appear in the summary buffer"
                    pidx))
      (dbs-goto-nth-summary prev)
      (dbs-set-index last))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement commands
;;;

(defsubst dbs-forward-record (arg)
  ;; Move point forward ARG records in the summary buffer,
  ;; remembering the start position of that record.
  (goto-char (edb--S :point))
  (db-forward-line-wrapping (* (edb--1D dbc-database :sum1lines) arg))
  (edb--S! :point (point)))

(defun dbs-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (if (not (with-current-buffer (edb--S :data-display-buffer)
             (not (edb--S :invisible-hidden-p))))
      (db-next-record-ignore-hiding arg)
    (dbs-synch-format-with-summary)
    (db-in-buffer (edb--S :data-display-buffer)
      (db-next-record-ignore-hiding arg))
    (dbs-forward-record arg)
    (dbs-set-index (with-current-buffer (edb--S :data-display-buffer)
                     (edb--S :index)))))

(defun dbs-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (dbs-next-record-ignore-hiding (- arg)))

(defun dbs-scroll-up ()
  (interactive)
  (scroll-up)
  (db-jump-to-point))

(defun dbs-scroll-down ()
  (interactive)
  (scroll-down)
  (db-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summary Mode commands
;;;

(defvar database-summary-mode-map nil
  "Keymap for database summary buffer.")

(unless database-summary-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    (suppress-keymap m)
    (mapc (lambda (key-def)
            (define-key m
              (let ((key (car key-def)))
                (if (consp key)
                    (concat meta (car key))
                  key))
              (cdr key-def)))
          '(("t" . toggle-truncate-lines)

            ;; Moving around in the database
            ("n"      . db-next-record)
            ("p"      . db-previous-record)
            ("\C-n"   . db-next-record)
            ("\C-p"   . db-previous-record)
            ("<"      . db-first-record)
            (">"      . db-last-record)
            (("<")    . db-first-record)
            ((">")    . db-last-record)
            ("j"      . db-jump-to-record)
            (" "      . db-next-screen-or-record)
            ("\177"   . db-previous-screen-or-record)
            (("n")    . dbs-next-record-ignore-hiding)
            (("p")    . dbs-previous-record-ignore-hiding)
            (("\C-n") . db-next-marked-record)
            (("\C-p") . db-previous-marked-record)

            ;; Exiting summary mode
            ("e" . dbs-edit)
            ("v" . dbs-view)
            ("q" . dbs-quit)
            ("x" . dbs-exit)

            ;; Adding and removing records
            ("a" . db-add-record)
            ("i" . db-add-record)
            ("d" . dbs-delete-record)
            ("k" . dbs-delete-record)
            ("o" . db-output-record-to-db)
            ("c" . db-copy-record)

            ;; Sorting
            ("S" . db-sort)

            ;; Searching commands
            ("s"    . db-search)
            ;;("S"  . db-incremental-search)
            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            ;; Everything else
            ("?"      . describe-mode)
            ("O"      . db-hide-record)
            (("o")    . db-hiding-toggle)
            (("O")    . db-hiding-set)
            (("\C-o") . db-toggle-show-hidden-records)
            ("g"      . db-summary)
            ("h"      . db-summary)
            ("D"      . db-summary)
            ("m"      . db-mark-record)
            ("r"      . db-report)
            ("\C-xr"  . db-revert-database)
            ("\C-v"   . dbs-scroll-up)
            (("v")    . dbs-scroll-down)

            ("\C-x\C-q" . db-toggle-modifiable-p)

            ("\C-c\C-c" . dbs-exit)

            ("b"   . undefined)
            ("f"   . undefined)
            ("l"   . undefined)
            ;;("u" . db-revert-record)
            ("w"   . undefined)
            ("y"   . undefined)
            ("z"   . undefined)))
    (setq database-summary-mode-map m)))


(defun dbs-view ()
  "Manipulate this record in the data display buffer in View mode."
  (interactive)
  (pop-to-buffer (edb--S :data-display-buffer))
  (db-view-mode))

(defun dbs-edit ()
  "Manipulate this record in the data display buffer in Edit mode."
  (interactive)
  (pop-to-buffer (edb--S :data-display-buffer))
  (when (eq 'database-view-mode major-mode)
    (db-first-field)))

(defun dbs-quit ()
  "Quit the summary buffer, and return to its data display buffer.
Delete any windows showing the summary buffer prior to burying it."
  (interactive)
  (let ((ddb (edb--S :data-display-buffer))
        (sum (current-buffer)))
    (delete-windows-on sum)
    (bury-buffer sum)
    (pop-to-buffer ddb)))

(defun dbs-exit ()
  "Exit the summary buffer, and return to its data display buffer.
Delete any windows showing the summary buffer prior to killing it."
  (interactive)
  (let ((ddb (edb--S :data-display-buffer))
        (sum (current-buffer)))
    (delete-windows-on sum)
    (kill-buffer sum)
    (pop-to-buffer ddb)))

(defun dbs-delete-record (&optional force)
  "Delete the current record from the database.
With a prefix argument, doesn't verify."
  (interactive "P")
  (when (or force (y-or-n-p "Delete this record? "))
    (db-in-buffer (edb--S :data-display-buffer)
      (db-delete-record t))
    ;; hope we're at the beginning of the record
    (let ((buffer-read-only nil))
      (kill-line (edb--1D dbc-database :sum1lines))
      (when (eobp)
        (goto-char (point-min))))
    (edb--S! :nrecords (1- (edb--S :nrecords)))
    (db-message "Record deleted")
    (dbs-set-index (with-current-buffer (edb--S :data-display-buffer)
                     (edb--S :index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

(defvar database-summary-mode-menu
  '("Database"
    "SUMMARY Mode:"
    ["Update summary"     db-summary t]
    ["Report"             db-report  t]
    "----"
    ("Motion"
     ["jump  to record"  db-jump-to-record t]
     ["last     record"  db-last-record    t]
     ["first    record"  db-first-record   t]
     "---"
     ["next"               db-next-record                 t]
     ["next (screen)"      db-next-screen-or-record       t]
     ["next (marked)"      db-next-marked-record          t]
     ["next (ingore hiding)" db-next-record-ignore-hiding t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen) "     db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ingore hiding)" db-previous-record-ignore-hiding t]
     "---"
     ["Isearch Backward" db-isearch-backward t]
     ["Isearch Forward" db-isearch-forward t]
     )
    "----"
    ["View record" dbs-view t]
    ["Edit record" dbs-edit t]
    ["Delete Record" dbs-delete-record t]
    ["Add Record" db-add-record t]
    ["Mark Record" db-mark-record t]
    ["Hide Record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    "----"
    ["Sort     database" db-sort                t]
    ["Revert   database" db-revert-database t]
    ["Save     database" db-save-database       t]
    ["Write    database" db-write-database-file t]
    "----"
    ["Quit" dbs-quit t]
    ["Exit" dbs-exit t]
    )
  "Menu for Database Summary mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-summary-mode-map
  "ignored-doc-string"
  database-summary-mode-menu)

;;; db-summary.el ends here

(provide 'edbcore)
