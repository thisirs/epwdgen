;;; epwdgen.el --- Flexible password generator

;; Copyright (C) 2017 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defvar epwdgen-password-presets
  '(("alphanumeric" password :letter mixed :number t :symbol nil :ambiguous t)
    ("passphrase, 4 words, space separator" passphrase :sep " ")
    ("upper+number, length 4" password :length 4 :letter uppercase-only :number t
     :symbol nil :ambiguous nil))
  "List of preset to generate passwords.

Each element is of the form (DESC SYMBOL [ARGS]) where DESC is a
string describing the preset and SYMBOL the function name to call
with arguments ARGS. SYMBOL can also be a symbol used with the
macro `epwdgen-define-generator'.")

(defconst epwdgen-letter-uppercase
  (loop for i from ?A to ?Z collect i))

(defconst epwdgen-letter-lowercase
  (loop for i from ?a to ?z collect i))

(defconst epwdgen-numbers
  (loop for i from ?0 to ?9 collect i))

(defconst epwdgen-symbols
  '(?! ?@ ?# ?$ ?% ?& ?* ?\( ?\) ?+ ?= ?/ ?{ ?} ?\[ ?\] ?: ?\; ?< ?> ?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))

(eval-and-compile
  (defun epwdgen--sanitize-args (args)
    (mapcar (lambda (e)
              (let ((sym (if (symbolp e) e (car e))))
                (list sym
                      (car (cdr-safe e))
                      (or (car (cdr (cdr-safe e)))
                          (intern (concat (symbol-name sym) "?"))))))
            args)))

(defmacro epwdgen-define-generator (name interactive-spec args &rest body)
  "Define a function to generate passwords.

A function `epwdgen-generate-password:NAME' is defined with
interactive spec INTERACTIVE-SPEC, arguments ARGS and body BODY.
When some argument is missing, ask for it."
  (declare (indent 2))
  (let* ((defun-sym (intern (concat "epwdgen-generate-password:" (symbol-name name))))
         (args (epwdgen--sanitize-args args))
         (keys (mapcar #'car args))
         (keys? (mapcar #'caddr args)))
    `(defun* ,defun-sym ,(cons '&optional (cons '&key args))
       (interactive)
       (let ((int-specs (if (stringp ',interactive-spec)
                            (split-string ,interactive-spec "\n")
                          ',interactive-spec))
             (keys? (list ,@keys?)) key? (keys ',keys) key int-spec)
         (while (setq key? (pop keys?) key (pop keys) int-spec (pop int-specs))
           (unless key?
             (set key (call-interactively
                       `(lambda (arg)
                         (interactive ,(if (stringp int-spec) int-spec
                                        `(list ,int-spec)))
                         arg)))))
         ,@body))))

;;;###autoload (autoload 'epwdgen-generate-password:password "epwdgen.el" nil t)
(epwdgen-define-generator password
    ("nLength? "
     (intern (completing-read "Letters? " '(uppercase-only lowercase-only mixed)))
     (yes-or-no-p "Numbers? ")
     (yes-or-no-p "Symbols? ")
     (yes-or-no-p "Remove ambiguous characters? ")
     (yes-or-no-p "No empty group? "))
  (length letter number symbol ambiguous group)
  "Return a string of LENGTH random characters.  If NUMBER is non-nil,
use numbers. If SYMBOL is non-nil, use one of \"!\"#$%&'()*+'-./:;<=>?@`{}|~\".
 If AMBIGUOUS is nil, avoid characters like \"l\" and \"1\", \"O\" and \"0\"."
  (let ((char-list
         (append
          (if (memq letter '(mixed uppercase-only))
              (loop for i in epwdgen-letter-uppercase
                    unless (member i (unless ambiguous '(?I ?O ?G)))
                    collect i))
          (if (memq letter '(mixed lowercase-only))
              (loop for i in epwdgen-letter-lowercase
                    unless (member i (unless ambiguous '(?l ?o)))
                    collect i))
          (and number (loop for i in epwdgen-numbers
                            unless (member i (unless ambiguous '(?0 ?1 ?6)))
                            collect i))
          (and symbol (loop for i in epwdgen-symbols
                            unless (member i (unless ambiguous '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\")))
                            collect i)))))
    (random t)
    (let (password)
      (while
          (progn
            (setq password (loop for i from 1 to length
                                 collect (nth (random (length char-list)) char-list)))
            (and group
                 (or (and number
                          (null (seq-intersection password epwdgen-numbers)))
                     (and (memq letter '(mixed lowercase-only))
                          (null (seq-intersection password epwdgen-letter-lowercase)))
                     (and (memq letter '(mixed uppercase-only))
                          (null (seq-intersection password epwdgen-letter-uppercase)))
                     (and symbol
                          (null (seq-intersection password epwdgen-symbols)))))))
      (setq password (apply #'string password))
      (if (called-interactively-p 'interactive)
          (insert password)
        password))))

;;;###autoload (autoload 'epwdgen-generate-password:passphrase "epwdgen.el" nil t)
(epwdgen-define-generator passphrase
    "nLength? \nsFile? \nsSep? "
  (length file sep)
  "Generate a passphrase of length LENGTH from words taken from
FILE and separated by SEP."
  (unless (executable-find "shuf") (error "Unable to find program `shuf'"))
  (let ((args (list "-n" (format "%s" length) (shell-quote-argument file)))
        return passphrase)
    (setq passphrase
          (with-temp-buffer
            (setq return (apply #'call-process "shuf" nil '(t t) nil args))
            (unless (= return 0)
              (error "Command failed: shuf %s:\n%s"
                     (mapconcat #'shell-quote-argument args " ")
                     (buffer-string)))
            (split-string (buffer-string))))
    (funcall (if (called-interactively-p 'interactive) 'insert 'identity)
             (mapconcat 'identity passphrase sep))))

;;;###autoload
(defun epwdgen-generate-password (method &rest args)
  "Generate a password"
  (interactive (list
                (let ((names (mapcar #'car epwdgen-password-presets)))
                  (completing-read "Password preset: " names nil t))))
  (let* ((preset (assoc method epwdgen-password-presets))
         (epwdgen-name (intern (concat "epwdgen-generate-password:"
                                       (symbol-name (nth 1 preset)))))
         (func-name (if (fboundp epwdgen-name) epwdgen-name (nth 1 preset))))
    (let ((password (apply func-name (append (cddr preset) args))))
      (if (called-interactively-p 'interactive)
          (insert password) password))))


(provide 'epwdgen)

;;; epwdgen.el ends here
