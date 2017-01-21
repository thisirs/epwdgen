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
  "List of preset to generate passwords.

Each element is of the form (DESC SYMBOL [ARGS]) where DESC is a
string describing the preset and SYMBOL the function name to call
with arguments ARGS. SYMBOL can also be a symbol used with the
macro `epwdgen-define-generator'."
  '(("passphrase, 4 words, space separator" passphrase
     :sep " " :file "/home/sylvain/CloudStation/Sylvain/wordlist.lst")
    ("upper+number, length 4" password :length 4 :upper t :number t :lower nil
     :symbol nil :ambiguous nil)))

(defun epwdgen--sanitize-args (args)
  (mapcar (lambda (e)
            (let ((sym (if (symbolp e) e (car e))))
              (list sym
                    (car (cdr-safe e))
                    (or (car (cdr (cdr-safe e)))
                        (intern (concat (symbol-name sym) "?"))))))
          args))

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
       (interactive ,interactive-spec)
       (let ((int-specs (split-string ,interactive-spec "\n"))
             (keys? (list ,@keys?)) key? (keys ',keys) key int-spec)
         (while (setq key? (pop keys?) key (pop keys) int-spec (pop int-specs))
           (unless key?
             (set key (call-interactively
                       `(lambda (arg) (interactive ,int-spec) arg)))))
         ,@body))))

(epwdgen-define-generator password
    "nLength? \nxNumber? \nxUpper? \nxLower? \nxSymbol? \nxAmbiguous? "
  (length number upper lower symbol ambiguous)
    "Return a string of LENGTH random characters.  If UPPER is non-nil,
use uppercase letters. If lower is non-nil, use lowercase
letters. If NUMBER is non-nil, use numbers. If SYMBOL is non-nil,
use one of \"!\"#$%&'()*+'-./:;<=>?@`{}|~\". If AMBIGUOUS is nil,
avoid characters like \"l\" and \"1\", \"O\" and \"0\"."
    (let ((char-list
           (append
            (and upper (loop for i from ?A to ?Z unless
                             (member i (unless ambiguous '(?I ?O ?G)))
                             collect i))
            (and lower (loop for i from ?a to ?z unless
                             (member i (unless ambiguous '(?l ?o)))
                             collect i))
            (and number (loop for i from ?0 to ?9 unless
                              (member i (unless ambiguous '(?0 ?1 ?6)))
                              collect i))
            (and symbol (loop for i = '(?! ?@ ?# ?$ ?% ?& ?* ?( ?) ?+ ?= ?/
                                           ?{ ?} ?[ ?] ?: ?\; ?< ?>)
                              unless (member i '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))
                              collect i)))))
      (random t)
      (apply 'string
             (loop for i from 1 to length
                   collect (nth (random (length char-list)) char-list)))))

(epwdgen-define-generator passphrase
    "nLength? \nsFile? \nsSep? "
  "Generate a passphrase of length LENGTH from words taken from
FILE and separated by SEP."
  (length file sep)
  (unless (executable-find "shuf") (error "Unable to find program `shuf'"))
  (let* ((shuf-cmd (format "shuf -n %d %s" length (shell-quote-argument file)))
         (pass-out (shell-command-to-string shuf-cmd)))
    (mapconcat 'identity (split-string pass-out) sep)))

;;;###autoload
(defun epwdgen-generate-password (method)
  "Generate a password"
  (interactive (list
                (let ((names (mapcar #'car epwdgen-password-presets)))
                  (completing-read "Password preset: " names nil t))))
  (let* ((preset (assoc method epwdgen-password-presets))
         (epwdgen-name (intern (concat "epwdgen-generate-password:"
                                   (symbol-name (nth 1 preset)))))
         (func-name (if (fboundp epwdgen-name) epwdgen-name (nth 1 preset))))
    (when (called-interactively-p 'interactive)
      (insert (apply fct-name (cddr preset))))
    (apply fct-name (cddr preset))))


(provide 'epwdgen)

;;; epwdgen.el ends here
