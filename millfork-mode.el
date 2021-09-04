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
;; Package-Requires: ((emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides basic syntax highlighting and editing capabilities for the Millfork
;; programmin language
;;
;;; Code:

(defconst millfork-font-lock-keywords
      (let* (
             (x-keywords '("if" "else" "for" "while" "do" "goto" "label" "return"
                           "to" "until" "downto" "parallelto" "parallel" "until"
                           "default" "break" "continue" "fast" "file" "defaultz"
                           "scr" "scrz" "import" "segment" "array" "struct"
                           "union" "enum" "alias" "inline"))
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


;;;###autoload
(define-derived-mode millfork-mode prog-mode "Millfork mode"
  "Major mode for editing Millfork"

  (setq font-lock-defaults '((millfork-font-lock-keywords))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mfk\\'" . millfork-mode))

(provide 'millfork-mode)
;;; millfork-mode.el ends here
