;; Line highlight and line number
(global-hl-line-mode t)
(global-linum-mode t)
;; Show column and line number 
(line-number-mode t)
(column-number-mode t)
;; Do not show the startup screen.
(setq inhibit-startup-message t)
;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
;(menu-bar-mode -1)
;(scroll-bar-mode -1)

;; ─────────────────────────────────── Set up 'package' ───────────────────────────────────
(require 'package)

;; Add melpa to package archives.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the packages are loaded before
;; you start trying to modify them.  This also sets the load path.
(package-initialize)
;; Install 'use-package' if it is not installed.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; ───────────────────────────────────Themes──────────────────────────────────
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'zenburn)
;(load-theme 'hc-zenburn t)


;; ───────────────────────────────────Load packages──────────────────────────────────

;; (use-package hc-zenburn-theme
;;   :config
;;   (load-theme (quote hc-zenburn) t))


(use-package company
  :ensure t
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.3)

  ;; Use company mode everywhere.
  (global-company-mode t))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

; Project management and tools
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
; Sidebar navigation with extras
(use-package treemacs
  :ensure t  
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode -1)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  :bind (("C-c C-g C-t" . treemacs)))
;(bind-key "C-c C-g C-t" treemacs)
; Unifies projectile and treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

; Makes treemacs show different colors for committed, staged and modified files
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; Git integration for Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package shell-pop
  :ensure t
  :bind (("C-t" . shell-pop))
)

;; (use-package shell-pop
;;   :bind (("C-t" . shell-pop))
;;   :config
;;   (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;;   (setq shell-pop-term-shell "/bin/zsh")
;;   ;; need to do this manually or not picked up by `shell-pop'
;;   (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; Git integration for Emacs
(use-package undo-tree
  :ensure t)

;; (use-package direnv
;;  :ensure t 
;;  :config
;;  (direnv-mode))

(use-package eyebrowse
              :ensure t
              :diminish eyebrowse-mode
              :config (progn
                        (define-key eyebrowse-mode-map (kbd "C-x C-1") 'eyebrowse-switch-to-window-config-1)
                        (define-key eyebrowse-mode-map (kbd "C-x C-2") 'eyebrowse-switch-to-window-config-2)
                        (define-key eyebrowse-mode-map (kbd "C-x C-3") 'eyebrowse-switch-to-window-config-3)
                        (define-key eyebrowse-mode-map (kbd "C-x C-4") 'eyebrowse-switch-to-window-config-4)
                        (eyebrowse-mode t)
                        (setq eyebrowse-new-workspace t)))

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "C-<") 'shrink-window-horizontally)
(global-set-key (kbd "C->") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c C-w w") 'ace-window)
(global-undo-tree-mode)

;; Move between windows: shit+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; Terminal:
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(bind-key "C-c C-g e" #'open-init-file)

(defun open-bash-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.bashrc"))

(bind-key "C-c C-g b" #'open-bash-file)

(defun open-terminal_tools-file ()
  "Open this very file."
  (interactive)
  (find-file "/mnt/work/lessOns/shScrambled/terminal.org"))

(bind-key "C-c C-g t" #'open-terminal_tools-file)


(use-package centaur-tabs
  :ensure t
  :demand
  ;; :config
  ;; (centaur-tabs-mode t)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  ;(centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)

  :bind
  (("C-x t" . #'centaur-tabs-backward)
   ("C-x y" . #'centaur-tabs-forward))
   ("C-c C-g a" . #'centaur-tabs-mode))

(use-package json-mode
  :ensure t)

(use-package fill-column-indicator
  :ensure t)
;;;https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
	     (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;(require 'fill-column-indicator)
(setq fci-rule-column 100)

(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-one t))

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                                                                    (modify-frame-parameters nil `((alpha . 100)))))

(require 'ido)
(ido-mode t)

;; workon home
(setenv "WORKON_HOME" "~/anaconda3/envs/")
(defalias 'workon 'pyvenv-workon)

;(setq python-shell-interpreter "/usr/bin/python3")
(setq elpy-rpc-backend "jedi")
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-virtualenv-path 'current)
;(setq exec-path (append exec-path '("/media/alvarocea/work/Programs/anaconda3/envs/sharpy_env/bin")))
;; ───────────────────────────────────Load packages──────────────────────────────────

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)
