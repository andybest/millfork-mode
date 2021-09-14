;;; millfork-mode.el --- Major mode for the Millfork programming language-*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Andy Best
;;
;; Author: Andy Best <https://github.com/andybest>
;; Created: September 04, 2021
;; Modified: September 04, 2021
;; Version: 0.0.1
;; Keywords: languages, millfork, 6502
;; Homepage: https://github.com/andybest/millfork-mode
;; Package-Requires: ((s "1.12.0") (emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides basic syntax highlighting and editing capabilities for the Millfork
;; programmin language
;;
;;; Code:

(require 's)

(defgroup millfork nil
  "Configuration for millfork-mode."
  :group 'languages
  :prefix "millfork-")

(defcustom millfork-indent-offset 4
  "Defines the indentation offser for Millfork code."
  :group 'millfork
  :type 'integerp)

(defconst millfork-font-lock-keywords
      (let* (
             (x-keywords '("if" "else" "for" "while" "do" "goto" "label" "return"
                           "to" "until" "downto" "parallelto" "parallel" "until"
                           "default" "break" "continue" "fast" "file" "defaultz"
                           "scr" "scrz" "import" "segment" "array" "struct"
                           "union" "enum" "alias" "inline" "const"))
             (x-types '("byte" "ubyte" "sbyte" "word" "long" "pointer" "void"
                        "bool" "set_carry" "set_zero" "set_overflow"
                        "set_negative" "clear_carry" "clear_zero"
                        "clear_overflow" "clear_negative" "int16" "int24" "int32"
                        "int40" "int48" "int56" "int64" "int72" "int80" "int88"
                        "int96" "int104" "int112" "int120" "int128" "unsigned8"
                        "signed8"))
             (x-constants '("true" "false"))
             (x-builtins '("not" "hi" "lo" "nonet" "sizeof" "sin" "cos" "tan"
                           "call" "defined"))
             (x-functions '())
             (x-preprocessor '("#if" "#elseif" "#else" "#endif" "#define" "#use"
                               "#pragma" "#fatal" "#error" "#warn" "#infoeval"
                               "#info"))

             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-builtins-regexp (regexp-opt x-builtins 'words))
             (x-functions-regexp (regexp-opt x-functions 'words))
             (x-preprocessor-regexp (regexp-opt x-preprocessor 'words)))
        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-builtins-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-preprocessor-regexp . font-lock-preprocessor-face)
          (,x-keywords-regexp . font-lock-keyword-face))))

(defconst millfork-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

                                        ; Treat braces as parens so we can (ab)use syntax-ppss for indentation
    (modify-syntax-entry ?\{ "($")
    (modify-syntax-entry ?\} ")^")

    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)

                                        ; Treat underscores as "word" characters
    (modify-syntax-entry ?_ "w" st)

                                        ; Comments
    (modify-syntax-entry ?\/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

;; ----- Intentation -----

(defun millfork--current-line ()
  "Return the text in the current line."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun millfork--effective-paren-depth (pos)
  "Calculate the paren depth of position POS, ignoring repeated parens on the same line."
  (let ((paren-depth 0)
        (syntax (syntax-ppss pos))
        (current-line (line-number-at-pos pos)))
    (save-excursion
                                        ; For each partial s-expression (paren enclosed expression)
      (while (> (nth 0 syntax) 0)
                                        ; Go to the character after the opening paren
        (goto-char (nth 1 syntax))

                                        ; If the paren is on a new line, increment the current paren depth.
        (let ((new-line (line-number-at-pos (point))))
          (unless (= new-line current-line)
            (setq paren-depth (1+ paren-depth))
            (setq current-line new-line)))

        (setq syntax (syntax-ppss (point)))))
    paren-depth))

(defun millfork--remove-comments (src)
  "Remove all comments from SRC."
  (replace-regexp-in-string (rx "//" (* not-newline)) "" src))

(defun millfork-indent-line ()
  "Indent the current line."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation)))
         (syntax-bol (syntax-ppss (line-beginning-position)))
         (current-paren-depth (millfork--effective-paren-depth (line-beginning-position)))
         (current-paren-pos (nth 1 syntax-bol))
         (text-after-paren
          (when current-paren-pos
            (save-excursion
              (goto-char current-paren-pos)
              (s-trim
               (millfork--remove-comments
                (buffer-substring
                 (1+ current-paren-pos)
                 (line-end-position)))))))
         (current-line (s-trim (millfork--current-line)))
         has-closing-paren)
                                        ; Unindent if the like starts a closing paren/brace
    (when (or (s-starts-with-p "}" current-line)
              (s-starts-with-p ")" current-line)
              (s-starts-with-p "]" current-line))
      (setq has-closing-paren t)
      (setq current-paren-depth (1- current-paren-depth)))

                                        ; Handle a negative paren depth (in the case of unbalance params)
    (when (< current-paren-depth 0)
      (setq current-paren-depth 0))

    (cond
     ((and (not (s-blank-str? text-after-paren))
           (not has-closing-paren))
      (let (open-paren-column)
        (save-excursion
          (goto-char current-paren-pos)
          (setq open-paren-column (current-column)))
        (indent-line-to (1+ open-paren-column))))

     (t
      (let ((indent-level current-paren-depth))
        (save-excursion

                                        ; Indent if a .methodCall
          (when (s-starts-with-p "." current-line)
            (setq indent-level (1+ indent-level))))
        (indent-line-to (* millfork-indent-offset indent-level)))))

    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

;;;###autoload
(define-derived-mode millfork-mode prog-mode "Millfork mode"
  "Major mode for editing Millfork"

  :group 'millfork
  :syntax-table millfork-mode-syntax-table

  (setq font-lock-defaults '((millfork-font-lock-keywords)))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local tab-width millfork-indent-offset)
  (setq-local indent-line-function #'millfork-indent-line)

  (setq-local electric-indent-chars
              (append "{}[]();,:" electric-indent-chars))
  
  (add-to-list 'compilation-error-regexp-alist 'millfork)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(millfork
                   "\\(?:ERROR\\|WARNIN\\(G\\)\\|INF\\(O\\)\\)\\: (\\(.*\\)\\:\\([0-9]+\\)\\:\\([0-9]+\\)) \\(.*\\)" 3 4 5 (1 . 2))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mfk\\'" . millfork-mode))

(provide 'millfork-mode)
;;; millfork-mode.el ends here
