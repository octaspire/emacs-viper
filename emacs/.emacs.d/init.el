;;; Copyright (c) 2022 octaspire.com
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
(defvar octaspire/config-dir (file-name-as-directory user-emacs-directory))
(defvar octaspire/wspace-sty '(face trailing tabs tab-mark))
(defvar octaspire/user-extras-file (concat octaspire/config-dir "extras.el"))
(defvar octaspire/emacs-evil-use-extras nil)

(setq
 auto-save-file-name-transforms        `((".*" ,temporary-file-directory t))
 backup-directory-alist                `((".*" . ,temporary-file-directory))
 column-number-mode                    t
 custom-file                           (concat octaspire/config-dir "custom.el")
 display-line-numbers-type             'relative
 explicit-shell-file-name              (if (file-exists-p "/usr/local/bin/bash") "/usr/local/bin/bash" "/bin/bash")
 ido-create-new-buffer                 'always
 ido-enable-flex-matching              t
 ido-everywhere                        t
 ido-file-extensions-order             '(".org" ".txt" ".md" ".c" ".h" ".cpp" ".hpp" ".py" ".mk" ".xml" ".ini" ".cfg")
 ido-ignore-extensions                 t ; Make Ido to use completion-ignored-extensions.
 ido-use-filename-at-point             'guess
 org-export-with-smart-quotes          1
 org-html-checkbox-type                'html
 org-html-doctype                      "html5"
 org-html-head-include-default-style   nil
 org-html-html5-fancy                  t
 org-html-htmlize-output-type          'inline-css
 org-html-postamble                    "Exported %T. &nbsp; | &nbsp; Modified %C.<br/>%c"
 org-replace-disputed-keys             t
 org-src-fontify-natively              1
 org-src-preserve-indentation          t
 user-full-name                        ""
 user-mail-address                     ""
 viper-mode                            t)

(require 'org)
(require 'term)
(require 'viper)

(when (eq system-type 'darwin)
  (setq mac-command-modifier    'meta
        mac-option-key-is-meta  nil
        mac-option-modifier     nil)
  (push "/usr/local/bin" exec-path))

(when (eq system-type 'berkeley-unix)
  (setq x-meta-keysym  'super
        x-super-keysym 'meta))

(load custom-file t) ; Load custom.el if present.

;; Set variable octaspire/emacs-evil-use-extras to t to initialize
;; the package system, install few packages, and start the Emacs
;; sever. If this variable is nil, no external packages are loaded.
;; A good place to change this variable is in file custom.el that
;; is not version controlled.
(when octaspire/emacs-evil-use-extras
  (load octaspire/user-extras-file))

(let ((aspell (executable-find "aspell")))
  (when aspell
    (setq ispell-program-name aspell)))

(unless
    (or (< (length user-full-name) 2)
        (< (length user-mail-address) 2))
  (setq org-html-postamble (concat org-html-postamble "<br/>%a %e")))

(setq-default
 calendar-week-start-day               1
 display-line-numbers-current-absolute t
 display-time-24hr-format              t
 indent-tabs-mode                      nil
 inhibit-startup-message               t
 large-file-warning-threshold          nil
 ring-bell-function                    'ignore
 show-trailing-whitespace              1
 tags-add-tables                       t
 tags-revert-without-query             t
 whitespace-style                      octaspire/wspace-sty)

(electric-pair-mode                    1)
(global-auto-revert-mode               1)
(global-display-line-numbers-mode      1)
(global-subword-mode                  +1)
(ido-mode                              1)
(menu-bar-mode                        -1)
(save-place-mode                       1)
(savehist-mode                         1)
(set-language-environment              "UTF-8")
(show-paren-mode                       1)
(which-function-mode                   1)
(global-whitespace-mode               +1)

(when window-system
  (scroll-bar-mode                    -1)
  (tool-bar-mode                      -1)
  (set-frame-size (selected-frame)     130 50))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun octaspire/init-file-open ()
  "Visit either user's Emacs initialization file, or the extra initialization."
  (interactive)
  (let ((bufname (buffer-file-name)))
    (if (and (stringp bufname)
             (file-truename bufname)
             (string= (file-truename bufname)
                      (file-truename user-init-file)))
        (find-file octaspire/user-extras-file) ; Initialization already visible, show extras.
      (find-file user-init-file))))

(defun octaspire/init-file-lint ()
  "Do simple linting of user's Emacs initialization files."
  (interactive)
  (let ((result1 (byte-compile-file user-init-file))
        (result2 (byte-compile-file octaspire/user-extras-file)))
    ;; Remove generated files.
    (delete-file (byte-compile-dest-file user-init-file))
    (delete-file (byte-compile-dest-file octaspire/user-extras-file))
    ;; Result is t if both files compiled cleanly.
    (and result1 result2)))

(defun octaspire/terminal-launch ()
  "Launch terminal (ansi-term) inside Emacs."
  (interactive)
  (ansi-term explicit-shell-file-name)
  (term-char-mode)
  (octaspire/term-enter-char-submode))

(defun octaspire/c-mode-hook ()
  "Set C/C++ coding style."
  (defvar c-basic-offset)  ; Get rid of warnings about
  (defvar c-default-style) ; assigning to free variable.
  (let ((spaces 2))
    (setq
     c-basic-offset   spaces
     c-default-style  "bsd"
     indent-tabs-mode nil
     tab-width        spaces)
    ;; Indent 'case' labels in switch statements.
    (c-set-offset 'case-label '+)
    ;; Don't indent '{' after 'if' (when on its own line).
    (c-set-offset 'substatement-open 0)
    ;; Continued lines should be indented by one depth.
    (c-set-offset 'statement-cont spaces)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'statement-block-intro '+)))

(defun fix-viper-del-backward-char-in-insert (orig-fun &rest args)
  "Fix viper backspace in term-mode's char submode."
  (interactive)
  (if (and (derived-mode-p 'term-mode)
           (eq viper-current-state 'insert-state)
           (term-in-char-mode))
      (term-send-raw-string "\C-h")
    (apply orig-fun args)))

(advice-add 'viper-del-backward-char-in-insert
            :around #'fix-viper-del-backward-char-in-insert)

(defun fix-viper-maybe-checkout (orig-fun &rest args)
  "Fix saving of GIT version controlled files during viper mode."
  (interactive)
  (when (and (featurep 'vc-hooks)
             (not (memq
                   (vc-backend
                    (expand-file-name (buffer-file-name)))
                   '(nil GIT))))
    (apply orig-fun args)))

(advice-add 'viper-maybe-checkout
            :around #'fix-viper-maybe-checkout)

(defun octaspire/term-enter-char-submode ()
  "Set style for (ansi-)term's char submode."
  (interactive)
  (display-line-numbers-mode     -1)
  (setq whitespace-style         nil
        show-trailing-whitespace nil))

(defun octaspire/term-enter-line-submode ()
  "Set style for (ansi-)term's line submode."
  (interactive)
  (display-line-numbers-mode     +1)
  (setq whitespace-style         nil
        show-trailing-whitespace nil))

(defun octaspire/term-toggle-submode ()
  "Toggle (ansi-)term mode(s) between char and line submodes."
  (interactive)
  (if (term-in-line-mode)
      (progn (term-char-mode)
             (octaspire/term-enter-char-submode))
      (progn (term-line-mode)
             (octaspire/term-enter-line-submode))))

(define-key term-mode-map (kbd "C-c C-j") 'octaspire/term-toggle-submode)
(define-key term-mode-map (kbd "C-c C-k") 'octaspire/term-toggle-submode)
(define-key term-raw-map  (kbd "C-c C-j") 'octaspire/term-toggle-submode)
(define-key term-raw-map  (kbd "C-c C-k") 'octaspire/term-toggle-submode)

(define-key org-mode-map  (kbd "C-c SPC") 'org-table-blank-field)

(global-set-key (kbd "C-c i") 'octaspire/init-file-open)
(global-set-key (kbd "C-c t") 'octaspire/terminal-launch)
(global-set-key [remap dabbrev-expand] 'hippie-expand) ; M-/ to expand anything

(add-hook 'text-mode-hook     'flyspell-mode)
(add-hook 'prog-mode-hook     'flyspell-mode)
(add-hook 'c-mode-common-hook 'octaspire/c-mode-hook)
(add-hook 'term-load-hook     (lambda ()
                                (term-line-mode)
                                (octaspire/term-enter-line-submode)))

(load-theme 'tango-dark t)
(set-face-attribute 'default nil :height 110)

(provide 'octaspire-init-el)
