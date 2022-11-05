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
(require 'package)
(require 'server)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package magit
  :ensure t)

(let ((sbcl (executable-find "sbcl"))
      (style "sbcl"))
  (when sbcl
    (use-package slime
      :ensure t
      :config
      (setq inferior-lisp-program     sbcl
            common-lisp-style         style
            common-lisp-style-default style))
    (use-package paredit
      :ensure t
      :config
      (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
      (add-hook 'slime-mode-hook      (lambda () (paredit-mode 1)))
      (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1))))))

(provide 'octaspire-extras-el)

(unless (server-running-p)
  (server-start))
