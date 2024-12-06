;; ========== Install Packages ===========================
(require 'package)

;; Add package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

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
    auto-indent-mode
    expand-region
    web-mode
    slim-mode
    haml-mode
    coffee-mode
    yaml-mode
    json-mode
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

(use-package neotree
  :bind ([f8] . neotree-toggle))

(use-package helm
  :config
  (require 'helm-autoloads)
  (helm-mode 1))

(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

(use-package smartparens
  :ensure smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package highlight-symbol
  :bind (([(control f3)] . highlight-symbol)
         ([f3] . highlight-symbol-next)
         ([(shift f3)] . highlight-symbol-prev)
         ([(meta f3)] . highlight-symbol-query-replace)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package auto-indent-mode
  :config
  (auto-indent-global-mode)
  (setq js-indent-level 2)
  (setq scss-indent-level 2))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.kml\\'" . nxml-mode))

(use-package slim-mode :init)

(use-package coffee-mode :init)

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package json-mode :init)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; org-mode ------------------------------------------------------------------
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
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
