;; NOTES:
;;-------
;; * The first time you load your configuration on a new machine,
;; you’ll need to run `M-x all-the-icons-install-fonts`
;; so that mode line icons display correctly.
;; ───────────────────────────────────General settings──────────────────────────────────
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
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; ─────────────────────────────────── Set up 'package' (old) ───────────────────────────────────
;; (require 'package)

;; ;; Add melpa to package archives.
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)
;; ;; (add-to-list 'package-archives
;; ;;              '("melpa" . "https://melpa.org/packages/") t)

;; ;; Load and activate emacs packages. Do this first so that the packages are loaded before
;; ;; you start trying to modify them.  This also sets the load path.
;; (package-initialize)
;; ;; Install 'use-package' if it is not installed.
;; (when (not (package-installed-p 'use-package))
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; ─────────────────────────────────── Set up 'package'  ───────────────────────────────────
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; ───────────────────────────────────Themes──────────────────────────────────
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'zenburn)
;(load-theme 'hc-zenburn t)

(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  :init (load-theme 'doom-one t))


;; ───────────────────────────────────Load packages──────────────────────────────────

;; ───────────────────────────────────
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

;; ───────────────────────────────────
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; ───────────────────────────────────
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Enable Flycheck
(when (require 'flycheck nil t)

  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;(setq flycheck-flake8rc "~/.config/flake8/setup.cfg")
;;(setq flycheck-pylintrc "~/.config/flake8/.pylintrc")
;; ───────────────────────────────────
; Project management and tools
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; ───────────────────────────────────
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

;; ───────────────────────────────────
;; Git integration for Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; ───────────────────────────────────
(use-package shell-pop
  :ensure t
  :bind (("C-t" . shell-pop))
)
;; (use-package shell-pop
;;   :bind (("C-t" . shell-pop))
;;   :config
;;   (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
;;   (setq shell-pop-term-shell "/bin/bash")
;;   ;; need to do this manually or not picked up by `shell-pop'
;;   (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; ───────────────────────────────────
;; Undo tree
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)

;; ───────────────────────────────────
;; (use-package direnv
;;  :ensure t 
;;  :config
;;  (direnv-mode))

;; ───────────────────────────────────
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

(global-set-key (kbd "C-c C-w w") 'ace-window)

;; ───────────────────────────────────
;; Move between windows: shit+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; Terminal:
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)

;; ───────────────────────────────────
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ───────────────────────────────────
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; ───────────────────────────────────
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
  (("<C-next>" . #'centaur-tabs-backward)
   ("<C-prior>" . #'centaur-tabs-forward))
   ("C-c C-g a" . #'centaur-tabs-mode))

;; ───────────────────────────────────
(use-package json-mode
  :ensure t)


(use-package command-log-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package origami
;;   :ensure t
;;   :commands (origami-mode)
;;   :bind (:map origami-mode-map
;;               ("C-c o :" . origami-recursively-toggle-node)
;;               ("C-c o a" . origami-toggle-all-nodes)
;;               ("C-c o t" . origami-toggle-node)
;;               ("C-c o o" . origami-show-only-node)
;;               ("C-c o u" . origami-undo)
;;               ("C-c o U" . origami-redo)
;;               ("C-c o C-r" . origami-reset)
;;               )
;;   :config
;;   (setq origami-show-fold-header t)
;;   ;; The python parser currently doesn't fold if/for/etc. blocks, which is
;;   ;; something we want. However, the basic indentation parser does support
;;   ;; this with one caveat: you must toggle the node when your cursor is on
;;   ;; the line of the if/for/etc. statement you want to collapse. You cannot
;;   ;; fold the statement by toggling in the body of the if/for/etc.
;;   (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
;;   :init
;;   (add-hook 'prog-mode-hook 'origami-mode)
;;   )

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

(defun hs-hide-all-comments ()
  "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-c-start-regexp "\\)")))
       (while (re-search-forward re (point-max) t)
         (if (match-beginning 1)
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))
;; ───────────────────────────────────
;; fci-mode 
(use-package fill-column-indicator
  :ensure t)
;; ───────────────────────────────────
;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; ───────────────────────────────────
;; org-mode
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-bullets
:ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; `with-eval-after-load' macro was introduced in Emacs 24.x
;; In older Emacsen, you can do the same thing with `eval-after-load'
;; and '(progn ..) form.
(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; ───────────────────────────────────
;; (require 'ido)
;; (ido-mode t)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

 (setq ivy-initial-inputs-alist nil)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

;; ───────────────────────────────────
;(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))
;; ───────────────────────────────────
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (define-key dired-mode-map
    (kbd "B") 'dired-single-up-directory)
  (define-key dired-mode-map
    (kbd "G") 'dired-single-buffer))

(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map
    (kbd "C-c h") 'dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ───────────────────────────────────
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell spell checking                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;enable flyspell in text mode (and derived modes)
(add-hook 'text-mode-hook 'flyspell-mode)
;;enable flyspell in languages comments
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;;cycle languages
(setq-default ispell-program-name "aspell")
(let ((langs '("en_GB" "en_US" "castellano")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)
;; ───────────────────────────────────Elisp functions──────────────────────────────


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

;;;;;;;;;;;;;;;;
;; emacsredux ;;
;;;;;;;;;;;;;;;;
(defun er-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
	 (read-string "Google: "))))))
 (global-set-key (kbd "C-c C-g C-g") #'er-google)


(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


(defun add-py-debug ()  
      "add debug code and move line down"  
    (interactive)  
    (move-beginning-of-line 1)  
    (insert "import pdb; pdb.set_trace();\n"))  

(define-key python-mode-map (kbd "C-c <f9>") 'add-py-debug)

(defun remove-py-debug ()  
  "remove py debug code, if found"  
  (interactive)  
  (let ((x (line-number-at-pos))  
    (cur (point)))  
    (search-forward-regexp "^[ ]*import pdb; pdb.set_trace();")  
    (if (= x (line-number-at-pos))  
    (let ()  
      (move-beginning-of-line 1)  
      (kill-line 1)  
      (move-beginning-of-line 1))  
      (goto-char cur))))  

;(local-set-key (kbd "<f9>") 'remove-py-debug)
(define-key python-mode-map (kbd "<f9>") 'remove-py-debug)
(define-key python-mode-map (kbd "<f8>") '(lambda ()  
                                 (interactive)   
                                 (search-forward-regexp "^[ ]*import pdb; pdb.set_trace();")   
                                 (move-beginning-of-line 1)))

;;; scrollers
;(global-set-key "\M-+" "\C-u1\C-v")
;(global-set-key "\M--" "\C-u1\M-v")
(global-set-key "\M--"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-+"  (lambda () (interactive) (scroll-down 4)) )
;; ───────────────────────────────────Environments──────────────────────────────

;; workon home
(setenv "WORKON_HOME" "~/anaconda3/envs/")
(defalias 'workon 'pyvenv-workon)

;(setq python-shell-interpreter "/usr/bin/python3")
(setq elpy-rpc-backend "jedi")
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-virtualenv-path 'current)
					;(setq exec-path (append exec-path '("/media/alvarocea/work/Programs/anaconda3/envs/sharpy_env/bin")))

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode t)

;; Set correct Python interpreter
(setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
(setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python"))))

;; ───────────────────────────────────Visit files──────────────────────────────

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
  (find-file "/mnt/work/develop/lessOns/shScrambled/terminal.org"))
(bind-key "C-c C-g t" #'open-terminal_tools-file)

;; ───────────────────────────────────Set command keys──────────────────────────────
(show-paren-mode 1)
(define-key comint-mode-map "\C-c\C-o" #'comint-clear-buffer)
(global-set-key (kbd "C-<") 'shrink-window)
(global-set-key (kbd "C->") 'enlarge-window)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)

;(require 'fill-column-indicator)
(setq fci-rule-column 100)
(bind-key "C-c C-g l" 'fci-mode)
(bind-key "C-c C-g m" 'menu-bar-mode)
(menu-bar-mode -1)

(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE")))

;; ───────────────────────────────────C++──────────────────────────────
;; snippets and snippet expansion
(use-package yasnippet
:ensure t
:init
(yas-global-mode 1))


;; tags for code navigation
(use-package ggtags
:ensure t
:config
(add-hook 'c-mode-common-hook
(lambda ()
(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
(ggtags-mode 1))))
)


;; ───────────────────────────────────Load packages──────────────────────────────────

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
