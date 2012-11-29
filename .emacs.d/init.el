;; PACKAGES
;;--------------------------------------------------

(require 'hippie-exp)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-eshell
                      haskell-mode
                      go-mode
                      clojure-mode
                      clojure-test-mode
                      color-theme
                      color-theme-solarized
                      nrepl
                      expand-region
                      elpy)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(package-initialize)

;; ENVIRONMENT
;;--------------------------------------------------

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

;; dir to store all extra extensions
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq vendor-dir (concat dotfiles-dir "/vendor"))

;; add my vendor dir to load path
(add-to-list 'load-path vendor-dir)

;; shove temp backup files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; CODING STYLES
;;--------------------------------------------------

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; smooth-scrolling stops that annoying jump when moving around
(require 'smooth-scrolling)

;; makes sexps flash when you eval them!
(require 'highlight)
(require 'eval-sexp-fu)

;; use inconsolata
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 120)

;; show line numbers
;; (when window-system (global-linum-mode t))

;; tabs are 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; solarized
(when window-system
  (require 'color-theme)
  (eval-after-load 'color-theme
    (progn
      (require 'color-theme-solarized)
      (color-theme-solarized-light))))

;; CLOJURE
;;--------------------------------------------------

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (contract 'defun)
     (defconstrainedfn 'defun)
     (defcontract 'defun)
     (provide 'defun)
     (log/log-time 'defun)
     (cli 'defun)
     (start 'defun)
     (stub-tradio 'defun)
     (stub-hitch 'defun)))

;; Python
;;--------------------------------------------------

(elpy-enable)
(setq python-check-command "pylint")

;; KEYBINDINGS
;;--------------------------------------------------

(defun osxp ()
  (string= "darwin" system-type))

(when (and (osxp) window-system)
  ;; make option the super key on mac
  (setq mac-option-modifier 'super)
  ;; map meta to command key for mac
  (setq ns-command-modifier 'meta))

(global-set-key [f7] 'ns-toggle-fullscreen)

;; steve yegges's suggested keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key [f5] 'call-last-kbd-macro)

;; where did this go in esk?
(global-set-key (kbd "C-x \\") 'align-regexp)
(put 'upcase-region 'disabled nil)

;; shift-arrow keys not cool in a terminal
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; TI specific stuff

(require 'ti-utils)
(require 'ti-services)
(setq ti-utils-src-dir "~/sandbox")
(put 'downcase-region 'disabled nil)

;; babel

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
                             (js . t)))
;; expand-region
(global-set-key (kbd "C-x 9") 'er/expand-region)

(setq js-indent-level 2)
