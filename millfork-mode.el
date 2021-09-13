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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides basic syntax highlighting and editing capabilities for the Millfork
;; programmin language
;;
;;; Code:

(require 'smie)

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
    ;(modify-syntax-entry ?\{ "(}" st)
    ;(modify-syntax-entry ?\} "){" st)

    (modify-syntax-entry ?\" "\"" st)

    (modify-syntax-entry ?_ "w" st)

    ; Comments
    (modify-syntax-entry ?\/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defconst millfork-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (id)
      (inst ("begin" insts "end")
            ("if" exp "then" inst "else" inst)
            (id ":=" exp)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("(" exps ")"))
      (exps (exps "," exps) (exp)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "+") (assoc "-")))))
   ;; (smie-merge-prec2s
   ;;  (smie-bnf->prec2
   ;;   '(
   ;;     (id)

   ;;     (decl-exp (id))

   ;;     (assign-exp (decl-exp) (id "=" exp))
   ;;     (decl (id ";" id))

   ;;     (func (id "{" insts "}"))
   ;;     (func-call (id "(" func-params ")"))
   ;;     (func-param (exp))
   ;;     (func-params (func-param "," func-param))

   ;;     (insts (inst) (inst ";" inst))
   ;;     (inst
   ;;      (decl)
   ;;      (func-call)
   ;;      (if-clause))

   ;;     (exp (op-exp))
   ;;     (op-exp (exp "OP" exp))

   ;;     (conditional (exp))
   ;;     (if-body ("if" conditional "{" insts "}"))
   ;;     (if-else-if (if-body) (if-else-if "else" if-else-if))
   ;;     (if-clause (if-else-if)))

   ;;     '((nonassoc "{") (assoc ",") (assoc ";"))
   ;;     '((assoc "OP"))
   ;;     '((assoc "else")))

   ;;  (smie-precs->prec2
   ;;   '(
   ;;     (left "*" "$*" "/" "%%")
   ;;     (left "+" "$+" "-" "$-" "|" "&" "^" ">>" "$>>" "<<" "$<<" ">>>>")
   ;;     (nonassoc ":")
   ;;     (nonassoc "==" "!=" "<" ">" "<=" ">=")
   ;;     (nonassoc "&&")
   ;;     (nonassoc "||"))))))

(defvar millfork-smie--operators-regexp
  (regexp-opt '("->" "*" "$*" "/" "%%" "+" "$+" "-" "$-" "|" "&" "^" ">>" "$>>"
                "<<" "$<<" ">>>>" ":" "==" "!=" "<" ">" "<=" ">=" "&&" "||")))

(defun millfork-smie--implicit-semicolon-p ()
  "Check whether the current statement has an implicit semicolon."
  (save-excursion
    (not (or (memq (char-before) '(?\{ ?\[ ?\,))
             (looking-back millfork-smie--operators-regexp (- (point) 3) t)))))

(defun millfork-smie--forward-token ()
  "Forward token function for smie lexer."
  (cond
   ((and (looking-at "\n") (millfork-smie--implicit-semicolon-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ";")
   ;; ((looking-at "{") (forward-char 1) "{")
   ;; ((looking-at "}") (forward-char 1) "}")
   ((looking-at millfork-smie--operators-regexp) ; Replace operators with "OP" token
    (goto-char (match-end 0)) "OP")
   (t (smie-default-forward-token))))

(defun millfork-smie--backward-token ()
  "Backward token function for smie."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (millfork-smie--implicit-semicolon-p))
      (message "implicit semi")
      ";")
     ;; ((eq (char-before) ?\{) (backward-char 1) "{")
     ;; ((eq (char-before) ?\}) (backward-char 1) "}")
     ((looking-back millfork-smie--operators-regexp (- (point) 3) t)
      (goto-char (match-beginning 0)) "OP")
     (t (smie-default-backward-token)))))

(defun verbose-millfork-smie-rules (kind token)
  "Verbose listing of rules."
  (let ((value (millfork-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s hanging:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

(defun millfork-smie-rules (kind token)
  "Smie indentation rules."
  (pcase (cons kind token)
    (`(:elem . basic) millfork-indent-offset)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . ":=") millfork-indent-offset)
    (`(:before . ,(or `"begin" `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent)))))
  ;; (pcase (;; cons kind token)
    ;; (`(:elem . basic) millfork-indent-offset)

    ;; (`(:after . "{")
    ;;  (if (smie-rule-parent-p "switch")
    ;;      (smie-rule-parent 0)))

    ;; (`(:before . ";")
    ;;  (if (smie-rule-parent-p "case" "default")
    ;;      (smie-rule-parent millfork-indent-offset)))

    ;; (`(:before . "if")
    ;;  (if (smie-rule-prev-p "else")
    ;;      (if (smie-rule-parent-p "{")
    ;;          millfork-indent-offset
           ;; (smie-rule-parent))))))

;;;###autoload
(define-derived-mode millfork-mode prog-mode "Millfork mode"
  "Major mode for editing Millfork"

  :group 'millfork
  :syntax-table millfork-mode-syntax-table

  (setq font-lock-defaults '((millfork-font-lock-keywords)))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local tab-width millfork-indent-offset)
  (setq-local indent-tabs-mode nil)

  (setq-local electric-indent-chars
              (append '(?. ?, ?: ?\) ?\] ?\}) electric-indent-chars))
  (smie-setup millfork-smie-grammar 'verbose-millfork-smie-rules
              :forward-fn 'millfork-smie--forward-token
              :backward-fn 'millfork-smie--backward-token))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mfk\\'" . millfork-mode))

(provide 'millfork-mode)
;;; millfork-mode.el ends here
