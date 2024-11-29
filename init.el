;; ========== Install Packages ===========================
(require 'package)

;; Add package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; Initialize the package system
(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))


;; Define a list of packages to install
(defvar my-packages
  '(
    zenburn-theme
    neotree
    projectile
    helm
    helm-projectile
    smartparens
    highlight-symbol
    multiple-cursors
    fastnav
    auto-indent-mode
    expand-region
    web-mode
    slim-mode
    haml-mode
    coffee-mode
    yaml-mode
    json-mode
    org
    ace-window
    corfu
    cape
    )
  "List of packages to install at startup.")

;; Function to install missing packages
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; ;; ;;-------------------------------------------------

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'helm)
;; (require 'helm-config)
(helm-mode 1)
;; ;; https://github.com/bbatsov/projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; http://tuhdo.github.io/helm-projectile.html
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(require 'smartparens-config)
(smartparens-global-mode 1)

(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'fastnav)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-r" 'fastnav-replace-char-forward)
(global-set-key "\M-R" 'fastnav-replace-char-backward)
(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)

(require 'auto-indent-mode)
(auto-indent-global-mode)
(setq js-indent-level 2)
(setq scss-indent-level 2)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'web-mode) ;;-------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; ;;---------------------------------------------------------------------

(require 'slim-mode)

(require 'coffee-mode)
(add-hook 'coffee-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 1)
            (set (make-local-variable 'indent-tabs-mode) t)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; (require 'haml-mode)
;; (add-hook 'haml-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; org-mode ------------------------------------------------------------------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;----------------------------------------------------------------------------

;; corfu -- ------------------------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t) ; Enable automatic completions
  (corfu-cycle t) ; Cycle through suggestions
  ;; :hook (prog-mode . corfu-mode) ; Enable in programming modes
  :init
  (global-corfu-mode)) ; Enable globally if needed

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )
;;----------------------------------------------------------------------------

;; ;;==========================================================================
;; ;;(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq coffee-tab-width 2)
(setq column-number-mode t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq ruby-insert-encoding-magic-comment nil)

(global-display-line-numbers-mode 1)
(load-theme 'zenburn t)
(tool-bar-mode -1)
(set-face-attribute 'default nil :height 110)

(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'speedbar-before-popup-hook (lambda () (linum-mode -1)))

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "M-I") 'vi-open-line-above)
(global-set-key (kbd "M-i") 'vi-open-line-below)

;; (setq make-backup-files nil) ; stop creating backup~ files
;; (setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil) ; stop creating #autosave# files

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; ;; ====================================================================================
;; ;; ====================================================================================

;; (add-to-list 'load-path "/home/andrew/.emacs.d/copilot.el")
;; (require 'copilot)

;; ;; (add-hook 'copilot-mode-hook
;; ;;   (lambda ()
;; ;;    (local-set-key kbd "C-c c-p a" 'copilot-accept-completion)))

;; ;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (defun copilot-start ()
;;   (interactive)
;;   (add-hook 'prog-mode-hook 'copilot-mode))

;; (defun copilot-stop ()
;;   (interactive)
;;   (remove-hook 'prog-mode-hook 'copilot-mode))

;; (global-set-key (kbd "C-x p c") 'copilot-complete)
;; (global-set-key (kbd "C-x p a") 'copilot-accept-completion)
;; (global-set-key (kbd "C-x p n") 'copilot-next-completion)
;; (global-set-key (kbd "C-x p p") 'copilot-previous-completion)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme yaml-mode web-mode smartparens slim-mode neotree multiple-cursors json-mode highlight-symbol helm-projectile haml-mode fastnav expand-region coffee-mode auto-indent-mode ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
