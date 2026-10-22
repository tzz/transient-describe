;;; transient-describe.el --- Self-describing, auto-packed transient menus  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.5"))
;; Keywords: convenience
;; URL: https://github.com/tzz/transient-describe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage: give a prefix the class `transient-describe-prefix'.  Its suffixes
;; then need no description (the command name is used), every suffix
;; shows the command's real key binding as a hint, and the columns of
;; each `transient-columns' group are re-flowed into as few rows as fit
;; the window.  So write the prefix as ONE outer group holding all the
;; columns and let the library decide where the rows break:
;;
;;   (transient-define-prefix my-menu ()
;;     "All of it"
;;     :class transient-describe-prefix
;;     [["Files"
;;       ("g" consult-git-grep)          ; shows: consult-git-grep (<f2>)
;;       ("d" "grep the tree" deadgrep)  ; shows: grep the tree (<f3>)
;;       ("f f" find-file)]              ; shows: find-file (C-x C-f)
;;      ["Buffer" ...]
;;      ["Toggles" ...]])
;;
;; The key hint is the shortest key bound to the command in the buffer
;; the transient was invoked from, ignoring transient's own keymaps and
;; menu-bar/tool-bar/mouse pseudo-bindings.  It's computed at display
;; time, so it tracks rebindings.  Descriptions are capped at
;; `transient-describe-max-width' so a single long name can't
;; make its column wrap.
;;
;; Packing is controlled by `transient-describe-pack-columns' and
;; `transient-describe-pack-order' (`height' packs tall columns together,
;; which minimises the total height; nil keeps your order).
;;
;; Independently of the class, every prefix is checked for duplicate
;; keys when it is defined (transient >= 0.5) and the first time it is
;; opened; duplicates surface as warnings.  `M-x
;; transient-describe-check-all-duplicates' audits everything at once.
;; Prefixes of other classes are otherwise untouched.
;;
;; Packages can register their commands into a column of such a menu
;; without editing the menu.  Use `transient-describe-register' directly,
;; or the `:transient' keyword this file adds to `use-package':
;;
;;   (use-package magit
;;     :transient "Git"                ; column title; created if new
;;     :bind (("C-x g" . magit-status) ...))
;;
;; Every command from the block's global `:bind' entries (not `:map'
;; ones) is appended to the column "Git" when the menu opens, after the
;; hand-written entries, with a generated transient key.  The menu must
;; put `transient-describe-setup-children' in its outer group:
;;
;;   (transient-define-prefix my-menu ()
;;     :class transient-describe-prefix
;;     [:class transient-columns
;;      :setup-children transient-describe-setup-children
;;      ["Files" ...] ["Git" ...] ...])
;;
;; A second registry is keyed by mode.  `transient-describe-register-mode'
;; (or `:transient (:mode MODE "Column")' in use-package) attaches a
;; column to a major or minor mode; a prefix whose outer group uses
;; `transient-describe-setup-mode-children' shows, at open time, the
;; columns of the current major mode and all its parents plus every
;; enabled minor mode.  Hand-written columns in that prefix are shown
;; always.  This is the "what can I do here" menu next to the global one.

;;; Code:

(require 'transient)
(require 'seq)
(require 'cl-lib)

(defgroup transient-describe nil
  "Auto descriptions, key hints and column packing for transient."
  :group 'transient)

(defclass transient-describe-prefix (transient-prefix) ()
  "Prefix whose suffixes get auto descriptions, key hints and packed columns.")

(defface transient-describe-key-hint
  '((t :inherit shadow))
  "Face for the global-keybinding hint appended to suffix descriptions.")

(defcustom transient-describe-max-width 40
  "Maximum width of a suffix description including the key hint, or nil.
Labels are truncated with an ellipsis to make room for the hint; if the
hint itself would leave less than `transient-describe-min-label-width' for
the label, the hint is dropped instead."
  :type '(choice (const :tag "Unlimited" nil) integer))

(defcustom transient-describe-min-label-width 18
  "Never truncate a label below this width just to fit the key hint."
  :type 'integer)

(defcustom transient-describe-pack-columns t
  "When non-nil, re-flow the columns of a group into rows that fit the window."
  :type 'boolean)

(defcustom transient-describe-pack-order 'height
  "How to order columns while packing.
`height' places tall columns together (first-fit decreasing), which
gives the shortest menu.  nil keeps the columns in definition order and
only breaks rows where the width runs out."
  :type '(choice (const :tag "Tallest first" height)
                 (const :tag "Definition order" nil)))

(defcustom transient-describe-pack-width nil
  "Width in columns to pack into, or nil to use the transient window's width."
  :type '(choice (const :tag "Automatic" nil) integer))

(defvar transient-describe-ignored-key-prefixes '(menu-bar tool-bar tab-bar remap)
  "Leading events that disqualify a binding from being shown as a hint.")

;;;; Key hints

(defvar transient-describe--key-cache nil
  "Alist (COMMAND . KEY) memoising `transient-describe--global-key' for one menu.")

(defun transient-describe--real-key-p (key)
  "Non-nil if KEY is something a user could type, not a menu/mouse pseudo-key."
  (and (> (length key) 0)
       (not (memq (aref key 0) transient-describe-ignored-key-prefixes))
       (not (string-match-p "mouse\\|wheel" (key-description key)))))

(defun transient-describe--global-key (command)
  "Return the shortest real key bound to COMMAND outside the transient, or nil."
  (if-let* ((hit (assq command transient-describe--key-cache)))
      (cdr hit)
    (let* ((buf (if (and (bound-and-true-p transient--original-buffer)
                         (buffer-live-p transient--original-buffer))
                    transient--original-buffer
                  (current-buffer)))
           (keys (with-current-buffer buf
                   ;; Transient installs its maps here; look past them.
                   (let ((overriding-terminal-local-map nil)
                         (overriding-local-map nil))
                     (where-is-internal command))))
           (keys (seq-filter #'transient-describe--real-key-p keys))
           (key (and keys
                     (key-description
                      (seq-reduce (lambda (best k)
                                    (if (< (length k) (length best)) k best))
                                  (cdr keys) (car keys))))))
      (push (cons command key) transient-describe--key-cache)
      key)))

(defun transient-describe--fit-description (label key)
  "Combine LABEL and hint KEY within `transient-describe-max-width'."
  (let* ((max transient-describe-max-width)
         (hint (and key (propertize (format "(%s)" key)
                                    'face 'transient-describe-key-hint)))
         (room (cond ((null max) nil)
                     (hint (- max (string-width hint) 1))
                     (t max))))
    (cond ((null max)
           (if hint (concat label " " hint) label))
          ((and hint (>= room transient-describe-min-label-width))
           (concat (truncate-string-to-width label room nil nil t) " " hint))
          ;; Hint too long to fit next to a readable label: drop it.
          (t (truncate-string-to-width label max nil nil t)))))

(cl-defmethod transient-format-description :extra "transient-describe" ((obj transient-suffix))
  "Fill in missing descriptions, append a key hint, cap the width.
Only active while a `transient-describe-prefix' is showing.  Infix arguments
and `:info' lines are left alone."
  (let ((desc (cl-call-next-method)))
    (if (not (and (cl-typep transient--prefix 'transient-describe-prefix)
                  (not (cl-typep obj 'transient-infix))
                  (slot-boundp obj 'command)
                  (symbolp (oref obj command))
                  (commandp (oref obj command))))
        desc
      (let ((cmd (oref obj command)))
        (transient-describe--fit-description (or desc (symbol-name cmd))
                                        (transient-describe--global-key cmd))))))

;;;; Column packing

(defvar transient-describe--packing nil
  "Non-nil while inserting the rows produced by packing; prevents recursion.")

(defun transient-describe--available-width ()
  "Columns available for one row of the menu."
  (or transient-describe-pack-width
      (if (window-live-p transient--window)
          (1- (window-width transient--window))
        ;; First display: the window doesn't exist yet.  Leave room for
        ;; fringes/scroll bar of the (usually full-width) side window.
        (- (frame-width) 4))))

(defun transient-describe--column-size (column parent)
  "Return (WIDTH . HEIGHT) of COLUMN as it will be rendered under PARENT."
  (transient--maybe-pad-keys column parent)
  (let* ((transient--pending-group column)
         (cells (flatten-tree (mapcar #'transient-format (oref column suffixes)))))
    (when-let* ((desc (transient-format-description column)))
      (push desc cells))
    (setq cells (mapcan (lambda (c) (split-string c "\n")) cells))
    (cons (apply #'max 0 (mapcar #'string-width cells))
          (length cells))))

(defun transient-describe--pack (columns parent avail)
  "Distribute COLUMNS of PARENT into rows no wider than AVAIL.
Returns a list of rows, each a list of column objects."
  (let* ((items (mapcar (lambda (c) (cons c (transient-describe--column-size c parent)))
                        columns))
         (items (if (eq transient-describe-pack-order 'height)
                    (seq-sort-by #'cddr #'> items) ; stable: ties keep order
                  items))
         (first-fit (eq transient-describe-pack-order 'height))
         (rows nil))                    ; each row: (USED-WIDTH . REVERSED-COLUMNS)
    (pcase-dolist (`(,col ,width . ,_) items)
      (let ((row (and rows
                      (if first-fit
                          (seq-find (lambda (r) (<= (+ (car r) 3 width) avail)) rows)
                        (and (<= (+ (car (car (last rows))) 3 width) avail)
                             (car (last rows)))))))
        (if row
            (progn (setcar row (+ (car row) 3 width))
                   (setcdr row (cons col (cdr row))))
          (setq rows (nconc rows (list (cons width (list col))))))))
    (mapcar (lambda (r) (nreverse (cdr r))) rows)))

(cl-defmethod transient--insert-group :around ((group transient-columns))
  "Re-flow GROUP's columns into rows that fit the window."
  (if (or transient-describe--packing
          (not transient-describe-pack-columns)
          (not (cl-typep transient--prefix 'transient-describe-prefix))
          transient-force-single-column
          (or (oref transient--prefix variable-pitch)
              transient-align-variable-pitch))
      (cl-call-next-method)
    ;; Mirror the `transient-group' :around, which we are bypassing.
    (when-let* ((desc (transient-format-description group)))
      (insert desc ?\n))
    (let* ((transient--pending-group group)
           (transient--max-group-level
            (max (oref group level) transient--max-group-level))
           (transient-describe--packing t)
           (rows (transient-describe--pack (oref group suffixes) group
                                      (transient-describe--available-width))))
      (while rows
        (transient--insert-group
         (transient-columns :suffixes (pop rows)
                            :level (oref group level)
                            :pad-keys (oref group pad-keys)))
        (when rows (insert ?\n))))))

;;;; Duplicate-binding detection

(defun transient-describe--suffix-plist (suffix)
  "The property list of a parsed SUFFIX, or nil if SUFFIX is not one.
Transient 0.8 parsed a suffix to (CLASS PLIST); the version bundled
with Emacs 32 parses it to (CLASS . PLIST).  Reading `cadr' on the
new shape returned the keyword `:key', every key lookup came back nil,
and the prefix-clash guard let \"T\" be both a command and a prefix
\(found 2026-10-21: <f3> signalled wrong-type-argument command keymap)."
  (when (and (consp suffix) (symbolp (car suffix)) (listp (cdr suffix)))
    (let ((rest (cdr suffix)))
      (cond ((keywordp (car rest)) rest)
            ((and (consp (car rest)) (keywordp (caar rest))) (car rest))))))

(defun transient-describe--layout-suffixes (layout)
  "Return (KEY . COMMAND) for every static suffix in LAYOUT, in order.
Groups are vectors [CLASS PLIST CHILDREN]; suffixes are read with
`transient-describe--suffix-plist'.  Dynamic groups (:setup-children)
can't be inspected and are skipped."
  (let (acc)
    (cl-labels ((walk (node)
                  (cond ((vectorp node)
                         (mapc #'walk (and (> (length node) 2) (aref node 2))))
                        ((consp node)
                         (let* ((plist (transient-describe--suffix-plist node))
                                (key (plist-get plist :key)))
                           (when (stringp key)
                             (push (cons (key-description (kbd key))
                                         (plist-get plist :command))
                                   acc)))))))
      (mapc #'walk layout))
    (nreverse acc)))

(defun transient-describe-check-duplicates (prefix &optional quiet)
  "Warn about keys bound more than once in transient PREFIX.
Also notes commands reachable under several keys (usually harmless, so
only reported at :debug level).  Returns non-nil if a duplicate key was
found.  With QUIET, return the result without warning."
  (interactive
   (list (intern (completing-read
                  "Check prefix: "
                  (let (ps) (mapatoms (lambda (s) (when (get s 'transient--layout)
                                                     (push s ps))))
                       ps)
                  nil t))))
  (let* ((pairs (transient-describe--layout-suffixes (get prefix 'transient--layout)))
         (by-key (seq-group-by #'car pairs))
         (by-cmd (seq-group-by #'cdr (seq-filter #'cdr pairs)))
         (dups (seq-filter (lambda (g) (cdr (cdr g))) by-key)))
    (unless quiet
      (pcase-dolist (`(,key . ,entries) dups)
        (display-warning
         'transient-describe
         (format "%s: key `%s' is bound %d times (%s); only the last one wins"
                 prefix key (length entries)
                 (mapconcat (lambda (e) (format "%s" (cdr e))) entries ", "))))
      (pcase-dolist (`(,cmd . ,entries) by-cmd)
        (when (cdr entries)
          (display-warning
           'transient-describe
           (format "%s: %s is bound under several keys: %s"
                   prefix cmd (mapconcat #'car entries ", "))
           :debug)))
      (when (and (called-interactively-p 'any) (null dups))
        (message "%s: no duplicate keys" prefix)))
    (and dups t)))

(defun transient-describe-check-all-duplicates ()
  "Run `transient-describe-check-duplicates' over every defined transient prefix."
  (interactive)
  (let ((n 0))
    (mapatoms (lambda (s)
                (when (get s 'transient--layout)
                  (when (transient-describe-check-duplicates s) (cl-incf n)))))
    (message "Checked transients; %d with duplicate keys" n)))

(defun transient-describe--check-once (prefix)
  "Check PREFIX for duplicates unless its current layout was already checked."
  (when (and (symbolp prefix) (get prefix 'transient--layout))
    (let ((layout (get prefix 'transient--layout)))
      (unless (eq layout (get prefix 'transient-describe--checked-layout))
        (put prefix 'transient-describe--checked-layout layout)
        (transient-describe-check-duplicates prefix)))))

;;;; Registered commands merged into columns

(defvar transient-describe--registry nil
  "Alist of (COLUMN-TITLE . COMMANDS) registered by packages.
Appended to the matching column by `transient-describe-setup-children'.")

(defcustom transient-describe-column-prefixes
  '(("Files" . "f") ("Buffer" . "b") ("Toggles" . "t") ("Info" . "i")
    ("Templates" . "y") ("Git" . "v") ("AI" . "I") ("Editing" . "e")
    ("Compile / errors" . "k") ("Windows / frames" . "W") ("Whitespace" . "w") ("Packages" . "P")
    ("Monk" . "K")
    ;; context menu columns
    ("Code" . "c") ("Elisp" . "l") ("Sexps" . "s") ("Dired" . "d")
    ("Summary" . "S") ("Article" . "A") ("Group" . "G") ("Message" . "m")
    ;; "T" is git-timemachine in the <f3> "This buffer" column
    ("Text" . "x") ("Markdown" . "M") ("Org" . "O") ("JSON" . "j"))
  "Transient key prefix used for commands appended to each column.
Columns not listed here get single-letter keys.  A prefix must not be a
complete key anywhere in the menu: transient cannot bind \"G\" to a
command and \"G t\" to another at the same time."
  :type '(alist :key-type string :value-type string))

;;;###autoload
(defun transient-describe-register (column &rest commands)
  "Append COMMANDS to the menu column titled COLUMN.
Idempotent: a command already registered under COLUMN is not added twice."
  (let ((cell (or (assoc column transient-describe--registry)
                  (car (push (cons column nil) transient-describe--registry)))))
    (dolist (cmd commands)
      (unless (memq cmd (cdr cell))
        (setcdr cell (append (cdr cell) (list cmd)))))
    (cdr cell)))

(defvar transient-describe--mode-registry nil
  "Alist of (MODE . ((COLUMN-TITLE . COMMANDS) ...)) registered per mode.
MODE is a major or minor mode symbol.  Used by
`transient-describe-setup-mode-children'.")

;;;###autoload
(defun transient-describe-register-mode (mode column &rest commands)
  "Append COMMANDS to column COLUMN shown when MODE is active.
MODE is a major mode (matched through `derived-mode-p', so a column for
`prog-mode' shows in every programming buffer) or a minor mode variable.
Idempotent per (MODE COLUMN COMMAND)."
  (let* ((mcell (or (assq mode transient-describe--mode-registry)
                    (car (push (cons mode nil) transient-describe--mode-registry))))
         (ccell (or (assoc column (cdr mcell))
                    (progn (setcdr mcell (append (cdr mcell) (list (cons column nil))))
                           (assoc column (cdr mcell))))))
    (dolist (cmd commands)
      (unless (memq cmd (cdr ccell))
        (setcdr ccell (append (cdr ccell) (list cmd)))))
    (cdr ccell)))

(defun transient-describe--active-modes ()
  "The current major mode, its parents, and enabled minor modes, in that order."
  (let ((majors nil) (m major-mode))
    (while m
      (push m majors)
      (setq m (get m 'derived-mode-parent)))
    (append (nreverse majors)
            (seq-filter (lambda (mm) (and (boundp mm) (symbol-value mm)))
                        (append local-minor-modes global-minor-modes)))))

(defun transient-describe--mode-columns ()
  "Alist (COLUMN-TITLE . COMMANDS) merged over the active modes.
Specific modes come first, so their commands lead in a shared column."
  (let (acc)
    (dolist (mode (transient-describe--active-modes))
      (pcase-dolist (`(,title . ,cmds) (cdr (assq mode transient-describe--mode-registry)))
        (let ((cell (or (assoc title acc)
                        (car (push (cons title nil) acc)))))
          (dolist (c cmds)
            (unless (memq c (cdr cell))
              (setcdr cell (append (cdr cell) (list c))))))))
    (nreverse acc)))

(defun transient-describe--column-title (group-spec)
  "Description string of the parsed group vector GROUP-SPEC, or nil."
  (and (vectorp group-spec) (> (length group-spec) 1)
       (plist-get (aref group-spec 1) :description)))

(defun transient-describe--spec-keys (spec)
  "Transient keys already used by the suffixes of parsed group SPEC."
  (let (keys)
    (dolist (child (and (> (length spec) 2) (aref spec 2)))
      (when-let* ((k (plist-get (transient-describe--suffix-plist child) :key)))
        (push k keys)))
    keys))

(defun transient-describe--free-key (command prefix used)
  "Pick a transient key for COMMAND under PREFIX not in USED.
Tries the first letters of the command name after its package prefix,
then the alphabet; returns \"PREFIX LETTER\" or just LETTER."
  (let* ((name (symbol-name command))
         (stem (if (string-match "\\`[^-]+-\\(.+\\)" name)
                   (match-string 1 name)
                 name))
         (candidates (append (delete-dups (mapcar #'string (string-to-list stem)))
                             (mapcar #'string (string-to-list "abcdefghijklmnopqrstuvwxyz"))))
         (candidates (seq-filter (lambda (c) (string-match-p "[a-z0-9]" c)) candidates)))
    (seq-some (lambda (letter)
                (let ((key (if prefix (concat prefix " " letter) letter)))
                  (and (not (member key used))
                       ;; a bare letter must not be the prefix of an existing key
                       (or prefix
                           (not (seq-some (lambda (u) (string-prefix-p (concat key " ") u))
                                          used)))
                       key)))
              (append candidates (mapcar #'upcase candidates)))))

(defvar transient-describe--used-keys nil
  "Keys taken anywhere in the menu being set up; bound during setup.")

(defun transient-describe--usable-prefix-p (key-prefix)
  "Non-nil if KEY-PREFIX is not itself a complete key in the menu."
  (not (member key-prefix transient-describe--used-keys)))

(defun transient-describe--column-with-registered (prefix spec)
  "Return SPEC with the commands registered for its title in PREFIX appended.
Keys are chosen against `transient-describe--used-keys', which covers the
whole menu, so a generated key never shadows or is shadowed by a hand
entry in another column."
  (let* ((title (transient-describe--column-title spec))
         (extra (and title (cdr (assoc title transient-describe--registry)))))
    (if (null extra)
        spec
      (let* ((present (mapcar (lambda (c) (plist-get (transient-describe--suffix-plist c) :command))
                              (aref spec 2)))
             (key-prefix (cdr (assoc title transient-describe-column-prefixes)))
             (key-prefix (cond ((null key-prefix) nil)
                               ((transient-describe--usable-prefix-p key-prefix) key-prefix)
                               (t (display-warning
                                   'transient-describe
                                   (format "column %S: prefix %S is already a key in this menu; using bare letters"
                                           title key-prefix))
                                  nil)))
             (new (delq nil
                        (mapcar (lambda (cmd)
                                  (unless (or (memq cmd present) (not (commandp cmd)))
                                    (when-let* ((key (transient-describe--free-key
                                                     cmd key-prefix
                                                     transient-describe--used-keys)))
                                      (push key transient-describe--used-keys)
                                      (transient-parse-suffix prefix (list key cmd)))))
                                extra))))
        (vector (aref spec 0) (aref spec 1) (append (aref spec 2) new))))))

(defun transient-describe--registered-only-columns (prefix children)
  "Column specs of PREFIX for registered titles that CHILDREN do not have.
A title with no entry in `transient-describe-column-prefixes' is given
one on the fly (a free upper-case letter), so its commands never take
bare keys that could collide with a hand column's prefix."
  (let ((have (delq nil (mapcar #'transient-describe--column-title children))))
    (delq nil
          (mapcar (lambda (entry)
                    (unless (member (car entry) have)
                      (unless (assoc (car entry) transient-describe-column-prefixes)
                        (let ((letter (seq-find
                                       (lambda (l)
                                         (and (not (member l transient-describe--used-keys))
                                              (not (seq-some (lambda (u) (string-prefix-p (concat l " ") u))
                                                             transient-describe--used-keys))))
                                       (mapcar #'string (string-to-list
                                                         (concat (upcase (substring (car entry) 0 1))
                                                                 "ZXQJVKYUNHRLDBCMPTSAEIOWG"))))))
                          (when letter
                            (push letter transient-describe--used-keys)
                            (setq-local transient-describe-column-prefixes
                                        (cons (cons (car entry) letter)
                                              transient-describe-column-prefixes)))))
                      (transient-describe--column-with-registered
                       prefix
                       (transient-parse-suffix prefix (vector (car entry))))))
                  transient-describe--registry))))

;;;###autoload
(defun transient-describe-setup-children (children)
  "`:setup-children' for an outer group of the symbol `transient-columns' class.
CHILDREN are the parsed children of the group.
Appends registered commands to their columns and adds columns for
titles that only registration knows about."
  (let ((prefix (oref transient--prefix command))
        (transient-describe--used-keys
         (mapcan (lambda (c) (and (vectorp c) (transient-describe--spec-keys c)))
                 children)))
    (append (mapcar (lambda (c) (transient-describe--column-with-registered prefix c))
                    children)
            (transient-describe--registered-only-columns prefix children))))

;;;###autoload
(defun transient-describe-setup-mode-children (children)
  "`:setup-children' for a context menu's outer group (symbol `transient-columns').
CHILDREN (hand-written columns) are kept and extended; then one column
per title registered for the active modes is added.  The registry is
consulted through a dynamic binding so the column merger can be reused."
  (let* ((prefix (oref transient--prefix command))
         (transient-describe--registry (transient-describe--mode-columns))
         (transient-describe--used-keys
          (mapcan (lambda (c) (and (vectorp c) (transient-describe--spec-keys c)))
                  children)))
    (append (mapcar (lambda (c) (transient-describe--column-with-registered prefix c))
                    children)
            (transient-describe--registered-only-columns prefix children))))

;;;; Global key reference

(defvar transient-describe--key-registry nil
  "Bindings recorded by `transient-describe-global-set-key'.
Each entry is (KEY COMMAND GROUP DESCRIPTION); KEY is a `key-description'
string.  Newest last.")

;;;###autoload
(defun transient-describe-global-set-key (key command &optional group description)
  "Bind KEY to COMMAND globally and record it for the key reference menu.
KEY is anything `global-set-key' accepts.  GROUP names the column the
binding is listed under (default \"Other\").  DESCRIPTION labels a
lambda; commands are labelled by name.  Rebinding the same KEY replaces
the earlier record."
  (global-set-key key command)
  ;; `key-description' understands both vectors and the unibyte-string
  ;; meta convention ("\M-d"); `kbd' on such a string yields "ä".
  (let* ((desc (key-description key))
         (entry (list desc command (or group "Other") description)))
    (setq transient-describe--key-registry
          (append (seq-remove (lambda (e) (equal (car e) desc))
                              transient-describe--key-registry)
                  (list entry)))
    command))

(defun transient-describe--key-columns ()
  "The key registry grouped by GROUP, in insertion order.
Result shape: ((GROUP . ((KEY COMMAND DESCRIPTION) ...)) ...)."
  (let (acc)
    (pcase-dolist (`(,key ,cmd ,group ,desc) transient-describe--key-registry)
      (let ((cell (or (assoc group acc) (car (push (cons group nil) acc)))))
        (setcdr cell (append (cdr cell) (list (list key cmd desc))))))
    (nreverse acc)))

;;;###autoload
(defun transient-describe-setup-key-children (children)
  "`:setup-children' listing every registered global key under its group.
Entries are `:info' lines -- text only, nothing to press -- showing the
real key and the command, so the menu is a cheat sheet.  Real keys
cannot double as suffix keys (\\`C-x' is transient's own prefix; \\`C-c'
both stands alone and starts longer keys).  CHILDREN (hand columns)
come first."
  (let ((prefix (oref transient--prefix command))
        (width (apply #'max 0 (mapcar (lambda (e) (string-width (car e)))
                                      transient-describe--key-registry))))
    (append
     children
     (mapcar
      (lambda (col)
        (transient-parse-suffix
         prefix
         (apply #'vector (car col)
                (mapcar
                 (pcase-lambda (`(,key ,cmd ,desc))
                   (list :info
                         (concat (propertize (format (format "%%-%ds" width) key)
                                             'face 'transient-key)
                                 "  "
                                 (or desc
                                     (and (symbolp cmd) (symbol-name cmd))
                                     (format "λ on %s" key)))))
                 (cdr col)))))
      (transient-describe--key-columns)))))

;;;; `use-package' :transient keyword

(defvar use-package-keywords)
(declare-function use-package-error "use-package-core")
(declare-function use-package-process-keywords "use-package-core")
(declare-function use-package-concat "use-package-core")
(declare-function use-package-list-insert "use-package-core")
(declare-function use-package--transient-bind-commands "transient-describe")

(with-eval-after-load 'use-package-core
  (defun use-package-normalize/:transient (_name keyword args)
    "Normalize to a list of specs: \"Column\" or (:mode MODE \"Column\").
A bare string registers the block's global binds in the global menu; a
(:mode MODE \"Column\") entry registers the block's binds for MODE in the
context menu.  Several specs may be given as a list."
    ;; use-package hands us the raw args list.  Accept:
    ;;   :transient "Col"                       -> ("Col")
    ;;   :transient (:mode M "Col")             -> ((:mode M "Col"))
    ;;   :transient ("Col" (:mode M "Col2") ...) -> that list
    (let ((specs (cond ((stringp (car args)) args)
                       ((eq (car-safe (car args)) :mode) args)
                       (t (car args)))))
      (dolist (spec specs)
        (unless (or (stringp spec)
                    (and (eq (car-safe spec) :mode) (symbolp (nth 1 spec))
                         (stringp (nth 2 spec))))
          (use-package-error
           (format "%s wants \"Column\" or (:mode MODE \"Column\"), got %S" keyword spec))))
      specs))

  (defun use-package--transient-bind-commands (binds &optional include-maps)
    "Commands bound by the normalized BINDS list.
Without INCLUDE-MAPS only global entries count; with it, `:map' entries too."
    (let (acc (in-map nil))
      (dolist (x binds)
        (cond ((eq x :map) (setq in-map 'next))
              ((eq in-map 'next) (setq in-map t))
              ((and (consp x) (or include-maps (not in-map))
                    (symbolp (cdr x)) (cdr x))
               (push (cdr x) acc))))
      (nreverse acc)))

  (defun use-package-handler/:transient (name _keyword specs rest state)
    "Register NAME's `:bind' commands in the global or a mode column."
    (let* ((body (use-package-process-keywords name rest state))
           (binds (plist-get rest :bind))
           (forms nil))
      (dolist (spec specs)
        (if (stringp spec)
            (when-let* ((cmds (use-package--transient-bind-commands binds)))
              (push `(transient-describe-register ,spec ,@(mapcar (lambda (c) `',c) cmds))
                    forms))
          ;; mode entry: mode-map bindings are exactly what belongs here
          (when-let* ((cmds (use-package--transient-bind-commands binds t)))
            (push `(transient-describe-register-mode ',(nth 1 spec) ,(nth 2 spec)
                                                     ,@(mapcar (lambda (c) `',c) cmds))
                  forms))))
      (use-package-concat (nreverse forms) body)))

  (unless (memq :transient use-package-keywords)
    ;; Anywhere is fine: the handler reads :bind from REST, it does not
    ;; depend on order.  Put it before :bind so it reads the normalized form.
    (setq use-package-keywords
          (use-package-list-insert :transient use-package-keywords :bind))))

;;;; Hooks into transient

(defun transient-describe--on-setup (&optional name &rest _)
  "Reset per-menu state and check NAME for duplicate keys."
  (setq transient-describe--key-cache nil)
  (transient-describe--check-once name))

(advice-add 'transient-setup :before #'transient-describe--on-setup)

;; Check at definition time when transient exposes the setter (>= 0.5).
(when (fboundp 'transient--set-layout)
  (advice-add 'transient--set-layout :after
              (lambda (prefix _layout) (transient-describe--check-once prefix))
              '((name . transient-describe-check-duplicates))))

(provide 'transient-describe)
;;; transient-describe.el ends here
