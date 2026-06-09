;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; WIP


;;; Code:
;;early background to prevent white emacs blinding me
(custom-set-faces
 '(default ((t (;;:foreground "white"
                ;;:background "#022b35"
                ))))
 '(bold ((t (:foreground "gold" :weight bold)))))

(global-unset-key (kbd "C-z"))
(setq-default fill-column 80)
(setq save-abbrevs 'silently)


;; Tree-sitter grammars are managed by home-manager.
;; The grammars are installed as Nix packages and exposed via
;; the EMACS_TREESIT_GRAMMAR_PATH environment variable.
(require 'treesit)
(let ((grammar-dir (getenv "EMACS_TREESIT_GRAMMAR_PATH")))
  (when (and grammar-dir (file-directory-p grammar-dir))
    (add-to-list 'treesit-extra-load-path grammar-dir)))

;; ENV VARIABLES
(defconst emacs-config-home user-emacs-directory)
(defconst emacs-config-home-writable (expand-file-name "~/.config/home-manager/configs/emacs/"))
(defconst emacs-cache-home (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))
(defconst emacs-data-home (expand-file-name "emacs/" (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defconst emacs-state-home (expand-file-name "emacs/" (or (getenv "XDG_STATE_HOME") "~/.local/state/")))

;; other directories

;; XDG Base Directory compliance for remaining packages
;; State files (long-term user preferences)
(setq recentf-save-file (concat emacs-state-home "recentf")
      abbrev-file-name (concat emacs-state-home "abbrev_defs"))

;; Data files (user-installed packages, user-generated content)
(setq bookmark-default-file (concat emacs-data-home "bookmarks")
      eshell-history-file-name (concat emacs-data-home "eshell/history")
      eshell-last-dir-ring-file-name (concat emacs-data-home "eshell/lastdir")
      package-user-dir (concat emacs-data-home "elpa"))

;; Cache files (performance data, can be regenerated)
(setq auto-save-list-file-prefix (concat emacs-cache-home "auto-save-list/.saves-")
      url-cookie-file (concat emacs-cache-home "url/cookies"))

;; Transient (Magit) - state and cache separation
(setq transient-history-file (concat emacs-state-home "transient/history.el")
      transient-levels-file (concat emacs-state-home "transient/levels.el")
      transient-values-file (concat emacs-state-home "transient/values.el"))

;; Projectile - separate cache and state
(setq projectile-known-projects-file (concat emacs-state-home "projectile-bookmarks.eld")
      projectile-cache-file (concat emacs-cache-home "projectile.cache"))
;; ========================================
;; package

;;(setq debug-on-error 't)

(defun init-read ()
  "Read the generated init file."
  (interactive)
  (find-file (concat emacs-config-home "init.el")))

(defun init ()
  "Edit your init file."
  (interactive)
  (find-file (concat emacs-config-home-writable "init.el")))

(setq package-archives
      '(("GNU ELPA"  . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")

        ;;("nongnu" . "https://elpa.nongnu.org/nongnu/")
        )
      ;; package-archive-priorities
      ;; '(("GNU ELPA"     . 10)
      ;;   ("MELPA"        . 5)
      ;;   ("MELPA Stable" . 0))
      )

;; Initialize the packages, avoiding a re-initialization.

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ========================================
;; Modes


(use-package all-the-icons
  :ensure t)
;;then run (all-the-icons-install-fonts) once

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-;" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :hook ((prog-mode . hs-minor-mode))
  :diminish hs-minor-mode
  :custom (hs-special-modes-alist
           (mapcar 'purecopy
                   '((c-mode "{" "}" "/[*/]" nil nil)
                     (c++-mode "{" "}" "/[*/]" nil nil)
                     (java-mode "{" "}" "/[*/]" nil nil)
                     (js-mode "{" "}" "/[*/]" nil)
                     (clojure-mode "[\(\[{]" "[\)\]}]" "#" nil nil)
                     (cider-repl-mode "[\(\[{]" "[\)\]}]" "#" nil nil)
                     (emacs-lisp-mode "\(" "\)" "#" nil nil)
                     (json-mode "{" "}" "/[*/]" nil)
                     (javascript-mode  "{" "}" "/[*/]" nil)))))



(use-package emacs-lisp-mode
  :no-require t
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "elisp")))))

(use-package clojure-snippets
  :ensure t)

(defvar clojure-mode-map (make-keymap))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.3))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :diminish (clojure-mode . "clj")
  :defines clojure-mode-map
  :bind (:map clojure-mode-map
         ("C-x t" . clojure-jump-to-test)
         ("C-c C-w" . cider-eval-last-sexp-and-replace)
         ("C-c M-e" . cider-eval-print-last-sexp)
         ("C-c j" . cider-jack-in-clj&cljs)
         ("C-:" . personal/insert-comment))
  :mode (("\\.edn$" . clojure-mode)
         ("\\.repl$" . clojure-mode)
         ("\\.bb$" . clojure-mode))
  :hook ((clojure-mode . (lambda () (setq mode-name "λ"))))
  :config
  (require 'flycheck-clj-kondo)

  (custom-set-faces
   '(font-lock-doc-face ((t (:foreground "#5B6268" :slant normal)))))

  (setq clojure-toplevel-inside-comment-form t)

  (defun ss/string-join (sep s)
    (mapconcat 'identity s sep))

  (setq safe-local-variable-values
	      (quote
	       ((eval define-clojure-indent
		            (snippet
		             (quote defun))
		            (template
		             (quote defun))))))

  (defun personal/insert-comment ()
    "Adds a rich comment block directly below the active top level form.
     source: dpsutton, clojurians slack"
    (interactive)
    (end-of-defun)
    (insert "\n")
    (insert "(comment\n  )\n")
    (clojure-backward-logical-sexp)
    (forward-char 1)
    (clojure-forward-logical-sexp)
    (insert "\n")
    (indent-according-to-mode)))

(use-package scheme
  :ensure t
  :diminish (scheme-mode . "scm")
  :defines scheme-mode-map
  :bind (:map scheme-mode-map
              ("C-c M-j" . run-guile)))

(global-set-key (kbd "s-z") 'zprint)

(use-package clj-refactor
  :ensure t
  :after clojure
  :diminish clj-refactor-mode
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-v")))
  :custom
  (cljr-clojure-test-declaration "[clojure.test :refer [is deftest testing]")
  (cljr-cljc-clojure-test-declaration
   "#?(:clj [clojure.test :refer [is deftest testing]]
   :cljs [cljs.test :refer [is deftest testing] :include-macros true])")
  :config
  (add-to-list 'cljr-magic-require-namespaces '("s" . "clojure.spec.alpha"))
  (add-to-list 'cljr-magic-require-namespaces '("pp" . "clojure.pprint"))
  (add-to-list 'cljr-magic-require-namespaces '("ig" . "integrant.core")))

(use-package paredit
  :ensure t
  :diminish (paredit-mode . " Ⓟ")
  :hook
  ((lisp-mode emacs-lisp-mode scheme-mode cider-repl-mode clojure-mode) . paredit-mode)

  :bind (:map clojure-mode-map
         ("C-c p" . paredit-mode)

         :map lisp-mode-map
         ("C-c p" . paredit-mode)

         :map paredit-mode-map
         ("C-c d" . duplicate-sexp)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("<M-left>" . paredit-backward)
         ("<M-right>" . paredit-forward))
  :init
  (defun duplicate-sexp ()
    "Duplicates the sexp at point."
    (interactive)
    (save-excursion
      (forward-sexp)
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (insert (concat (buffer-substring (car bounds) (cdr bounds)) "\n"))
        (indent-for-tab-command)))))

(use-package sgml-mode
  :bind (:map sgml-mode-map
         ("<f1> SPC" . sgml-mark-tag))
  :mode (("\\.html$" . sgml-mode)
         ("\\.xml$" . sgml-mode))
  :config
  (defun sgml-mark-tag ()
    (interactive)
    (if (= 60 (char-after))
        (progn
          (sgml-skip-tag-forward 1)
          (set-mark-command nil)
          (sgml-skip-tag-backward 1))
      (mark-sexp))))

(use-package paren
  :init
  (custom-set-faces
   '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold)))))
  :hook ((lisp-mode . show-paren-mode)
         (emacs-lisp-mode . show-paren-mode)
         (scheme-mode . show-paren-mode)
         (cider-repl-mode . show-paren-mode)
         (clojure-mode . show-paren-mode)))

(use-package paren-face ;; dims parentheses
  :defer t
  :custom
  (paren-face-regexp "[][(){}]"))

(use-package cider
  :ensure t
  :diminish (cider-mode . " ⓒ")
  :bind (:map cider-mode-map
         ("C-c M-o" . cider-repl-clear-buffer)
         ("C-x M-e" . cider-pprint-eval-last-sexp-to-repl)
         ("C-c C-x" . cider-ns-refresh)
         ("<f2>" . clojure-quick-eval)
         :map cider-repl-mode-map
         ("C-c C-x" . cider-ns-refresh)
         :map cider-start-map
         ("C-c C-x" . cider-ns-refresh)
         )
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)

  :config
  (setq cider-repl-history-file (concat emacs-data-home "cider-history"))
  (setq cider-font-lock-dynamically nil)
  (setq cider-repl-history-size 10000)
  (setq cider-repl-buffer-size-limit 100000)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-inject-dependencies-at-jack-in t)
  (setq nrepl-prompt-to-kill-server-buffer-on-quit nil)
  (setq cider-repl-result-prefix ";; => ")

  ;; Try to replicate this workflow: https://github.com/clojure-emacs/cider/issues/2617
  (setq cider-invert-insert-eval-p t)
  (setq cider-repl-pop-to-buffer-on-connect nil)

  (setq clojure-quick-sexp
   '("(dev/reset)" "(user/fix)" "(use 'clojure.repl)"
     "(use 'clojure.tools.trace)" "(use 'clojure.pprint)"
     "(dev/start-cljs-figwheel)"))

  (set-face-attribute 'cider-test-failure-face nil :background "#8c2020"))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))
  :config
  (defconst projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
  (defconst projectile-globally-ignored-files '("TAGS" ".nrepl-port"))
  (defconst projectile-globally-ignored-file-suffixes '("pyc"))
  (defconst projectile-globally-ignored-directories
    '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "venv" "build"
      "vendor" "vendors" ".cabal-sandbox" "dist" ".vagrant" "node_modules"
      "bower_components" ".bundle" ".stack-work"))
  (defconst projectile-completion-system 'ivy)
  (defconst projectile-remember-window-configs t)
  (defconst projectile-use-git-grep t)
  (defconst projectile-create-missing-test-files t)
  (projectile-mode 1))

(use-package terraform-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-load-directory (concat user-emacs-directory "snippets")))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  ;;:bind (("<tab>" . company-complete))
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        ;; M-<num> to select an option according to its number.
        company-show-numbers t
        ;; Do not downcase completions by default.
        company-dabbrev-downcase nil
        ;; Even if I write something with the wrong case,
        ;; provide the correct casing.
        company-dabbrev-ignore-case t
        ;; No company-mode in shell & eshell
        company-global-modes '(not eshell-mode shell-mode)
        ;; Only 2 letters required for completion to activate.
        company-minimum-prefix-length 3
        ;; Wait 0.1s before completion.
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
        company-backends  '((company-capf :with company-dabbrev-code)
                            company-dabbrev)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("TAB" . company-complete-selection)
        ("C-g" . company-abort))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode))
  ;; :init
  ;; (global-company-mode)
  ;; (setq company-minimum-prefix-length 2)
  ;; (setq company-begin-commands
  ;;       '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash cljr-slash))
  )

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :config
  (counsel-mode)
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :after ivy
  ;; pin to MELPA because MELPA-Stable has bug
  ;; gets rid of ivy-rich-set-display-transformer: Symbol’s value as variable is void: ivy--display-transformers-lis
  :pin MELPA
  :diminish
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :diminish
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch))

(use-package org
  :ensure t
  :bind (:map org-mode-map
         ("<S-insert>" . org-complete)
         ("<S-return>" . org-insert-subheading)
         ("<s-return>" . org-insert-subheading)
         ("C-c a" . org-agenda)
         ("C-c s" . ss/standup-slack))
  :init
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) " . (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "→"))))
                            ("\\<\\(FIXME\\):" 1 'font-lock-warning-face prepend)
                            ;;("\\<\\(and\\|or\\|not\\)\\>" . 'font-lock-keyword-face)
                            ))
  :custom
  (org-agenda-files (list "~/notes/personal.org" "~/notes/bsq.org"))
  (org-babel-hash-show-time t)
  (org-clock-display-default-range 'untilnow)
  (org-clock-into-drawer nil)
  (org-confirm-babel-evaluate nil)
  (org-confirm-elisp-link-function nil)
  (org-directory "~/notes")
  (org-duration-format '(("h" . t) ("min" . t)))
  (org-ellipsis "↴")
  (org-export-babel-evaluate nil)
  (org-export-backends '(ascii html icalendar latex md odt))
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-html-htmlize-output-type 'css)
  (org-image-actual-width nil)
  (org-mobile-inbox-for-pull "~/notes/flagged.org")
  (org-outline-path-complete-in-steps nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)              ; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)           ; Preserving indentation in source blocks
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-table-convert-region-max-lines 999)
  (org-todo-keyword-faces '(("PROG" . "yellow")
                            ("CNCL" . "IndianRed1")
                            ("BLOK" . "IndianRed1")))
  (org-todo-keywords '((sequence "TODO" "PROG" "BLOK" "CNCL" "DONE")))
  (org-special-ctrl-a/e t)
  :config
  (defconst org-roam-v2-ack t)
  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"440\""
            " height=\"335\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

  (org-link-set-parameters
   "yt"
   :follow
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   :export
   (lambda (path desc backend)
     (cl-case backend
       (html (format yt-iframe-format
                     path (or desc "")))
       (latex (format "\href{%s}{%s}"
                      path (or desc "video"))))))

  (org-link-set-parameters
   "j"
   :follow
   (lambda (id)
     (browse-url
      (concat "https://bare-square.atlassian.net/browse/" id))))

  (org-link-set-parameters
   "vb"
   :follow
   (lambda (id)
     (browse-url
      (concat "https://bare-square.atlassian.net/browse/VB-" id))))

  (org-link-set-parameters
   "CVE"
   :follow
   (lambda (id)
     (browse-url
      (concat "https://nvd.nist.gov/vuln/detail/CVE-" id))))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure    . t)
                                 (css        . t)
                                 (dot        . t)
                                 (emacs-lisp . t)
                                 (java       . t)
                                 (js         . t)
                                 (perl       . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (shell      . t)
                                 (sql        . t)))

  ;; Custom org-babel Java execution via JShell (defined here for org-babel)
  (defun org-babel-execute:jshell (body params)
    "Execute Java code in BODY via JShell with PARAMS."
    (let ((tmp-file (make-temp-file "jshell-" nil ".java")))
      (with-temp-file tmp-file
        (insert body))
      (let ((result (shell-command-to-string
                     (format "echo 'load(\"%s\")' | jshell -" tmp-file))))
        (delete-file tmp-file)
        result)))

  ;; Note: We don't add jshell to org-babel-load-languages because
  ;; org-babel would try to load ob-jshell.el which doesn't exist
  ;; The org-babel-execute:jshell function above will be called automatically

  ;; (defun sql-to-org-table ()
  ;;   (interactive)
  ;;   (mc/edit-lines)
  ;;   (org-force-self-insert "|")
  ;;   (multiple-cursors-mode))

  (defun ss/standup-slack (start end)
    (interactive "r")
    (let ((oldbuf (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring oldbuf start end)
        (goto-char (point-min))
        (replace-match " BLOK" " :todo-pause:")
        (goto-char (point-min))
        (replace-match " DONE" " :todo-done:")
        (goto-char (point-min))
        (replace-match " PROG" " :todo-doing:")
        (goto-char (point-min))
        (replace-match " CNCL" " :todo-cancel:")
        (goto-char (point-min))
        (replace-match " TODO" " :todo:")
        (goto-char (point-min))
        (replace-match "^\\* " "") ; remove header stars and space
        (goto-char (point-min))
        (replace-match "^\\*" "") ; remove item stars
        (kill-ring-save (point-min) (point-max)))))

  (defun ss/html-from-org (beg end)
    (interactive "r")
    (narrow-to-region beg end)
    (org-html-export-to-html)
    (browse-url (concat temporary-file-directory (org-export-output-file-name ".html")))
    (widen))

  (set-face-attribute 'org-hide nil :foreground "DarkSlateGray")
  (set-face-attribute 'org-link nil :foreground "CornflowerBlue")
  (set-face-attribute 'org-link nil :underline t)
  (font-lock-add-keywords
   'org-mode `(("^\\*+ \\(TODO\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "□") nil)))
               ("^\\*+ \\(PROG\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "▶") nil)))
               ("^\\*+ \\(BLOK\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "⏸") nil)))
               ("^\\*+ \\(CNCL\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘") nil)))
               ("^\\*+ \\(DONE\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔") nil)))))
  (let* ((ss/variable-font-tuple (list :font "DejaVu Sans Mono"))
         (ss/fixed-font-tuple    (list :font "DejaVu Sans Mono" :height 1.0))
         (base-font-color        "grey65")
         (background-color       (face-background 'default nil 'default))
         (primary-color          (face-foreground 'mode-line nil))
         (secondary-color        (face-background 'secondary-selection nil 'region))
         (base-height            (face-attribute 'default :height))
         (headline               `(:inherit default :weight regular :foreground ,base-font-color)))

    (custom-set-faces `(org-agenda-structure ((t (:inherit default :height 2.0 :underline nil))))
                      `(org-verbatim ((t (:inherit 'fixed-pitched :foreground "#aef"))))
                      `(org-table ((t (:inherit 'fixed-pitched))))
                      `(org-block ((t (:inherit 'fixed-pitched))))
                      `(org-block-background ((t (:inherit 'fixed-pitched))))
                      `(org-block-begin-line ((t (:background "gray15" :foreground "gray28" :slant normal))))
                      `(org-block-end-line ((t (:background "gray15" :foreground "gray28" :slant normal))))

                      `(org-ellipsis ((t (:foreground "dim gray"))))

                      ;;levels
                      `(org-level-8 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-7 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-6 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-5 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-4 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-3 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-2 ((t (,@headline ,@ss/fixed-font-tuple))))
                      `(org-level-1 ((t (,@headline ,@ss/fixed-font-tuple))))

                      `(org-document-title ((t (,@headline ,@ss/variable-font-tuple :height 1.5 :underline nil)))))))


(use-package org-tree-slide
  :ensure t
  ;; :bind (("<f12>" . org-tree-slide-mode)
	;;  ("<S-f12>" . org-tree-slide-skip-done-toggle))
  :init (org-tree-slide-simple-profile))

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory "~/notes/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      ;;#'org-roam-capture--get-point
      "* %?"
      :file-name "~/notes/daily/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d>\n\n"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  (package-check-signature nil)
  (org-roam-db-location (concat emacs-state-home "org-roam.db"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         (:map org-mode-map
               ("C-<" . org-roam-dailies-goto-previous-note)
               ("C->" . org-roam-dailies-goto-next-note)))
  :init
  ;; Suppress native compilation warnings for org-roam
  (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (setq native-comp-async-report-warnings-errors 'silent))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-superstar
  :ensure t
  :custom (org-bullets-bullet-list '("●"))
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-jira
  :ensure t
  :custom
  (jiralib-url "https://bare-square.atlassian.net")
  (org-jira-working-dir (concat emacs-data-home "org-jira/"))
  ;;override request backend (curl) to fix [error] request--curl-sync: semaphore never called
  (request-backend 'url-retrieve)
  (story-points-field 'customfield_10004)
  :bind (("C-c j s" . org-jira-get-summary)
         ("C-c j p" . org-jira-show-story-points)
         ("C-c j P" . org-jira-insert-story-points))
  :config
  (defun org-jira-get-field (jira-id field)
    (unless jira-id (error "ORG_JIRA_ERROR: JIRA-ID missing in org-jira-get-field!"))
    (cdr (assoc field (assoc 'fields (car (org-jira-get-issue-by-id jira-id))))))

  (defun org-jira-show-story-points ()
    "Get issue story points from point"
    (interactive)
    (let ((jira-id (thing-at-point 'symbol)))
      (message "%s" (org-jira-get-field jira-id story-points-field))))

  (defun org-jira-insert-story-points ()
    "Get issue story points from point and place next to issue id from jira"
    (interactive)
    (let ((jira-id (thing-at-point 'symbol)))
      (forward-symbol 1)
      (insert (format " - %s" (org-jira-get-field jira-id story-points-field))))))

;;; end of org

(use-package with-editor
  :ensure t) ;;needed by magit

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :custom-face
  (magit-blame-date ((t (:background "#404040" :foreground "#F2804F"))))
  (magit-blame-heading ((t (:background "#404040" :foreground "#073642"))))
  (magit-blame-name ((t (:inherit magit-blame-heading :background "#404040" :foreground "#F2804F"))))
  (magit-blame-summary ((t (:background "#404040" :foreground "#F2804F" :weight bold))))
  (magit-diff-file-heading-highlight ((t (:background "#073642" :weight semi-bold))))
  (magit-diff-hunk-heading ((t (:background "#009F00" :foreground "black"))))
  (magit-diff-hunk-heading-highlight ((t (:background "#5FFF5F" :foreground "black"))))
  (magit-popup-argument ((t (:foreground "white"))))
  (smerge-refined-added ((t (:inherit smerge-refined-change :background "#227022"))))
  :custom
  (git-commit-finish-query-functions nil)
  (git-commit-summary-max-length 120)
  (magit-diff-refine-hunk t)
  (magit-log-margin '(t "%Y-%m-%d " magit-log-margin-width t 18))
  :hook ((magit-mode-hook . (lambda ()
                              (setq fill-column 72)
                              (auto-fill-mode t))))
  :config
  (global-set-key (kbd "C-c C-g") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package blamer
  :ensure t
  :after magit
  :defer 20
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 40)
  (blamer-author-formatter " ✎ %s ")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :height 0.7
                   :italic t)))
  ;; :config
  ;; (global-blamer-mode 1)
  )

(use-package forge
  :ensure t
  :after magit
  :defer t
  :init
  ;; hack to allow forge to work on guix emacs
  ;; TODO: remove after checking it's not necessary on guix emacs
  (unless (boundp 'bug-reference-auto-setup-functions)
    (defvar bug-reference-auto-setup-functions '())))

;; ========================================
;; Navigation

(use-package dired
  :demand t
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (set-face-attribute 'dired-marked nil :foreground "#5fff00")
  (if window-system
      (set-face-attribute 'dired-directory nil :foreground "#5fd7ff")
    (set-face-attribute 'dired-directory nil :foreground "#0020ff"))

  (defun dired-copy-filename ()
    (interactive)
    (setq current-prefix-arg '(0))
    (call-interactively 'dired-copy-filename-as-kill))

  (defun kill-dired-buffers ()
    (interactive)
    (mapc (lambda (buffer)
	    (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	      (kill-buffer buffer)))
	  (buffer-list))))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :init
  (global-set-key (kbd "C-,") 'symbol-overlay-jump-prev)
  (global-set-key (kbd "C-.") 'symbol-overlay-jump-next)
  :config
  (custom-set-faces
   '(symbol-overlay-default-face ((t (:foreground "gray100" :background "#9c7618" :weight semi-bold)))))
  (add-hook 'prog-mode-hook 'symbol-overlay-mode))

(use-package windmove
  :init
  (global-set-key (kbd "H-j") 'windmove-left)
  (global-set-key (kbd "H-l") 'windmove-right)
  (global-set-key (kbd "H-i") 'windmove-up)
  (global-set-key (kbd "H-k") 'windmove-down)

  (global-set-key (kbd "C-x <left>")  'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <up>")    'windmove-up)
  (global-set-key (kbd "C-x <down>")  'windmove-down))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (let ((undo-tree-history-directory (concat emacs-data-home "undo-tree")))
    (setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-directory))))
  :config
  (global-undo-tree-mode)

  (custom-set-faces
   '(undo-tree-visualizer-active-branch-face ((t (:background "#002b36" :foreground "gray95" :weight bold))))))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-x .") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x ,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x /") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-S-x C-S-x") 'mc/edit-lines)
  (defun mce ()
    (interactive)
    (mc/edit-lines)))

(use-package expand-region
  :ensure t
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

;; bookmarks
(use-package bm
  :ensure t
  :bind (("C-c C-b" . bm-toggle)
         ("<s-up>" . bm-previous)
         ("<s-down>" . bm-next))
  :init
  (custom-set-faces
   '(bm-face ((t (:background "#007994"))))))

(use-package tiling
  :init
  (global-set-key (kbd "C-\\") 'tiling-cycle))

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(use-package deadgrep
  :ensure t
  :bind ("<f9>" . deadgrep))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :bind (("<M-up>" . markdown-move-list-item-up)
         ("<M-down>" . markdown-move-list-item-down))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

;; github
(use-package browse-at-remote
  :bind (("C-c g g" . browse-at-remote))
  :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected t)
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
               `(:host ,(rx bol "gitlab.ithaca.ece.uowm.gr" eol)
                       :type "gitlab")))

(use-package restclient
  :ensure t
  :mode (("\\.http$" . restclient-mode)))

;;;========================================
;;; Python
;;;========================================

(use-package python
  :ensure t
  :delight "Py"
  :bind (:map python-mode-map
              ("C-c M-j" . run-python) ;; "jack-in"
              ("C-c C-k" . python-shell-send-buffer)
              ("C-x C-e" . python-shell-send-statement))
  :hook ((python-mode . (lambda ()
                          (define-key python-mode-map "\"" 'electric-pair)
                          (define-key python-mode-map "\'" 'electric-pair)
                          (define-key python-mode-map "(" 'electric-pair)
                          (define-key python-mode-map "[" 'electric-pair)
                          (define-key python-mode-map "{" 'electric-pair)
                          (setq mode-name "Py"))))
  ;; Remove guess indent python message
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :config
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython"
	          python-shell-interpreter "ipython"
	          python-shell-interpreter-args "-i --simple-prompt")))
   ((setq python-shell-interpreter "python")))

  (defun electric-pair ()
    "If at end of line, insert character pair without surrounding spaces.
    Otherwise, just insert the typed character."
    (interactive)
    (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1))))

;; Hide the modeline for inferior python processes (as well as R)
(use-package inferior-python-mode
  :ensure nil
  :hook ((inferior-python-mode . hide-mode-line-mode)
	       (inferior-ess-r-mode . hide-mode-line-mode)))

(use-package hide-mode-line
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :pin "GNU ELPA")

(use-package pythonic
  :ensure t)

(use-package uv-mode
  :ensure t
  :hook (python-mode . uv-mode-auto-activate-hook))

(use-package python-pytest
  :ensure t
  :after python
  :bind (:map python-mode-map
	      ("C-c C-t n" . python-pytest-file-dwim)
        ("C-c C-t t" . python-pytest-function-dwim)
        ("C-c C-t s" . python-pytest-dispatch))
  :custom
  (python-pytest-arguments
   '(;;"--v"            ;; show verbose output
     "--color"        ;; colored output in the buffer
     "--failed-first" ;; run the previous failed tests first
     "--maxfail=5"    ;; exit in 5 continuous failures in a run
  )))

(use-package python-black
  :ensure t
  :defer t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Buffer formatting on save
;; (use-package yapfify
;;   :diminish yapf-mode
;;   :ensure t
;;   :defer t
;;   :hook (python-mode . yapf-mode))

;; numpy docstring for python
(use-package numpydoc
  :ensure t
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))


;;;========================================
;;; Java
;;;========================================

;; JShell REPL integration for interactive Java development
;; Note: ob-jshell is not available in MELPA, so we'll use basic Java support
;; and custom JShell functions instead
;; (org-babel jshell function defined earlier in org configuration)

;; Enhanced Java REPL mode
(require 'comint)  ; Required for JShell REPL functionality

(defvar jshell-buffer-name "*JShell*"
  "Name of the JShell buffer.")

(defvar jshell-prompt-regexp "^jshell> "
  "Regexp for matching JShell prompts.")

(defun jshell-start ()
  "Start JShell REPL in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create jshell-buffer-name)))
    (unless (get-buffer-process buffer)
      (with-current-buffer buffer
        (let ((classpath (jshell-get-project-classpath)))
          (if classpath
              (make-comint-in-buffer "JShell" buffer "jshell" nil
                                   "--class-path" classpath)
            (make-comint-in-buffer "JShell" buffer "jshell" nil)))
        (setq mode-line-process '(":%s"))
        (comint-mode)
        (setq comint-prompt-regexp jshell-prompt-regexp)
        (setq comint-use-prompt-regexp t)))
    (pop-to-buffer buffer)))

(defun jshell-get-project-classpath ()
  "Get project classpath for JShell."
  (when (buffer-file-name)
    (let ((project-root (or (locate-dominating-file (buffer-file-name) "pom.xml")
                           (locate-dominating-file (buffer-file-name) "build.gradle")
                           (locate-dominating-file (buffer-file-name) "build.gradle.kts"))))
      (when project-root
        (cond
         ;; Maven project - use dependency:build-classpath + target/classes
         ((file-exists-p (concat project-root "pom.xml"))
          (let ((cp-file (concat temporary-file-directory "jshell-classpath.txt")))
            (shell-command (format "cd %s && mvn dependency:build-classpath -Dmdep.outputFile=%s -q" project-root cp-file))
            (if (file-exists-p cp-file)
                (let ((dep-classpath (string-trim (with-temp-buffer
                                                    (insert-file-contents cp-file)
                                                    (buffer-string)))))
                  (delete-file cp-file)
                  (concat dep-classpath ":" project-root "target/classes"))
              ;; Fallback if dependency command fails
              (concat project-root "target/classes"))))

         ;; Gradle project - simplified approach
         ((or (file-exists-p (concat project-root "build.gradle"))
              (file-exists-p (concat project-root "build.gradle.kts")))
          (let ((gradle-classes (concat project-root "build/classes/java/main")))
            (shell-command-to-string (format "cd %s && gradle compileJava -q >/dev/null 2>&1 && echo %s" project-root gradle-classes))))

         ;; Fallback: just return classes directory
         (t nil))))))

(defun jshell-start-with-project ()
  "Start JShell with project compilation first."
  (interactive)
  (when (buffer-file-name)
    (let ((project-root (or (locate-dominating-file (buffer-file-name) "pom.xml")
                            (locate-dominating-file (buffer-file-name) "build.gradle")
                            (locate-dominating-file (buffer-file-name) "build.gradle.kts"))))
      (when project-root
        (message "Compiling project first...")
        (cond
         ((file-exists-p (concat project-root "pom.xml"))
          (shell-command (format "cd %s && mvn compile -q" project-root)))
         ((or (file-exists-p (concat project-root "build.gradle"))
              (file-exists-p (concat project-root "build.gradle.kts")))
          (shell-command (format "cd %s && gradle compileJava -q" project-root))))
        (message "Project compiled. Starting JShell..."))))
  (jshell-start))

(defun jshell-send-region (start end)
  "Send region to JShell."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (jshell-send-string code)))

(defun jshell-send-string (string)
  "Send STRING to JShell process."
  (let ((proc (get-buffer-process jshell-buffer-name)))
    (if proc
        (progn
          (comint-send-string proc (concat string "\n"))
          (display-buffer jshell-buffer-name))
      (message "No JShell process running. Start with C-c M-j"))))

(defun jshell-send-buffer ()
  "Send entire buffer to JShell."
  (interactive)
  (jshell-send-region (point-min) (point-max)))

(defun jshell-eval-last-expression ()
  "Evaluate the Java expression before point in JShell."
  (interactive)
  (let ((end (point))
        (start (save-excursion
                 (backward-sexp)
                 (point))))
    (jshell-send-region start end)))

(defun jshell-eval-print-last-expression ()
  "Evaluate and print the Java expression before point."
  (interactive)
  (let ((expr (buffer-substring-no-properties
               (save-excursion (backward-sexp) (point))
               (point))))
    (jshell-send-string (concat "System.out.println(" expr ");"))))

(defun java-scratch-buffer ()
  "Create a Java scratch buffer for quick experimentation."
  (interactive)
  (let ((buffer (generate-new-buffer "*Java Scratch*")))
    (with-current-buffer buffer
      (java-mode)
      (insert "// Java Scratch Buffer - Evaluate with C-x C-e\n")
      (insert "// Start JShell with C-c M-j\n")
      (insert "// Switch to JShell with C-c C-z\n\n")
      (insert "import java.util.*;\n")
      (insert "import java.time.*;\n")
      (insert "import java.util.stream.*;\n")
      (insert "import java.nio.file.*;\n")
      (insert "import java.util.concurrent.*;\n\n")
      (insert "// Quick examples:\n")
      (insert "// List.of(1, 2, 3, 4, 5).stream().filter(x -> x % 2 == 0).toList()\n")
      (insert "// LocalDateTime.now()\n")
      (insert "// \"Hello World\".chars().mapToObj(c -> (char) c).toList()\n\n"))
    (pop-to-buffer buffer)))

;; Java REPL utilities for better REPL-driven development
(defun java-send-defun ()
  "Send the current Java method or class to JShell."
  (interactive)
  (save-excursion
    (mark-defun)
    (jshell-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun java-eval-and-replace ()
  "Evaluate Java expression and replace it with the result."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
         (expr (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (jshell-send-string expr)))

(defun jshell-clear-buffer ()
  "Clear JShell buffer."
  (interactive)
  (when (get-buffer jshell-buffer-name)
    (with-current-buffer jshell-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer))
      (comint-send-string (get-buffer-process jshell-buffer-name) "/reset\n"))))

;; Quick access to Java documentation
(defun java-search-javadoc ()
  "Search Java documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (browse-url (concat "https://docs.oracle.com/en/java/javase/21/docs/api/java.base/"
                       "java/util/List.html"))))  ; This would need to be enhanced for real symbol lookup

;; Enable which-function-mode for Java method detection
(add-hook 'java-mode-hook 'which-function-mode)

;; Enhanced Maven integration with REPL-style testing
(use-package maven-test-mode
  :ensure t
  :config
  (add-hook 'java-mode-hook 'maven-test-mode)

  ;; Enhanced test running functions
  (defun maven-test-single ()
    "Run single test method at point."
    (interactive)
    (let* ((method-name (which-function))
           (class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (test-command (format "mvn test -Dtest=%s#%s" class-name method-name)))
      (compile test-command)))

  (defun maven-test-file ()
    "Run all tests in current file."
    (interactive)
    (let* ((class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (test-command (format "mvn test -Dtest=%s" class-name)))
      (compile test-command)))

  (defun maven-test-all ()
    "Run all tests in project."
    (interactive)
    (compile "mvn test"))

  (defun maven-run-main ()
    "Run main class."
    (interactive)
    (let ((main-class (read-string "Main class: ")))
      (compile (format "mvn exec:java -Dexec.mainClass=%s" main-class))))

  ;; Auto-compile on save for faster feedback
  (defun java-auto-compile ()
    "Automatically compile Java file on save."
    (when (and (eq major-mode 'java-mode)
               (buffer-file-name)
               (string-match "\\.java$" (buffer-file-name)))
      (let ((compilation-read-command nil))
        (save-window-excursion
          (compile "mvn compile -q")))))

  ;;(add-hook 'after-save-hook 'java-auto-compile)
  )

;; Gradle integration with enhanced features
(use-package gradle-mode
  :ensure t
  :config
  (add-hook 'java-mode-hook 'gradle-mode)

  ;; Gradle test running functions
  (defun gradle-test-single ()
    "Run single test method at point."
    (interactive)
    (let* ((method-name (which-function))
           (class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (test-command (format "gradle test --tests %s.%s" class-name method-name)))
      (compile test-command)))

  (defun gradle-test-file ()
    "Run all tests in current file."
    (interactive)
    (let* ((class-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (test-command (format "gradle test --tests %s" class-name)))
      (compile test-command)))

  (defun gradle-run-main ()
    "Run main application with Gradle."
    (interactive)
    (compile "gradle run")))

;; XML support for Maven/Gradle files
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.pom\\'" . nxml-mode))
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 2))

;; JSON support
(use-package json-mode
  :mode "\\.json\\'")

;; Shows a transient menu for the selected prefix after some delay
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

;;; `lsp-mode` proper
;;; The config should be relatively agnostic up to here

(use-package lsp-mode
  :ensure t
  :defer t
  :delight " LSP"
  :defines (lsp-keymap-prefix lsp-mode-map)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((java-mode . lsp)
         (java-mode . (lambda ()
                        ;; Java REPL-style key bindings (similar to CIDER)
                        (local-set-key (kbd "C-c M-j") 'jshell-start-with-project) ; project-aware jshell
                        (local-set-key (kbd "C-c j") 'jshell-start)            ; basic jshell
                        (local-set-key (kbd "C-x C-e") 'jshell-eval-last-expression)
                        (local-set-key (kbd "C-c C-k") 'jshell-send-buffer)
                        (local-set-key (kbd "C-c C-r") 'jshell-send-region)
                        (local-set-key (kbd "C-c C-z") (lambda () (interactive) (pop-to-buffer jshell-buffer-name)))
                        (local-set-key (kbd "C-c C-w") 'java-eval-and-replace)  ; eval and replace like Clojure
                        (local-set-key (kbd "C-c M-e") 'jshell-eval-print-last-expression)
                        (local-set-key (kbd "C-c C-d") 'java-send-defun)       ; send function/method
                        (local-set-key (kbd "C-c s") 'java-scratch-buffer)     ; open scratch buffer
                        (local-set-key (kbd "C-c M-o") 'jshell-clear-buffer)   ; clear REPL like cider
                        (local-set-key (kbd "C-c C-h") 'java-search-javadoc)   ; documentation lookup
                        ;; Test running shortcuts
                        (local-set-key (kbd "C-c C-t t") 'maven-test-single)
                        (local-set-key (kbd "C-c C-t f") 'maven-test-file)
                        (local-set-key (kbd "C-c C-t p") 'maven-test-all)
                        ;; Build shortcuts
                        (local-set-key (kbd "C-c C-c") 'compile)               ; compile project
                        (local-set-key (kbd "C-c C-x") (lambda () (interactive) ; restart/refresh
                                                          (jshell-clear-buffer)
                                                          (when (get-buffer-process jshell-buffer-name)
                                                            (kill-process (get-buffer-process jshell-buffer-name)))
                                                          (jshell-start)))))
          (typescript-mode . lsp-deferred)
          (tsx-tsx-mode . lsp-deferred)
          (javascript-mode . lsp-deferred)
          (js-jsx-mode . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map ("M-<RET>" . lsp-execute-code-action))
  :config
  (setq gc-cons-threshold 1600000
        lsp-completion-enable t
        lsp-completion-enable-additional-text-edit nil
        lsp-eldoc-enable-hover nil
        lsp-enable-snippet t
        lsp-enable-symbol-highlighting t
        lsp-enable-links t
        lsp-keep-workspace-alive nil
        lsp-restart 'auto-restart
        lsp-signature-auto-activate nil
        read-process-output-max (* 1024 1024)

        ;; LSP workspace management - no blocklists, manual project management
        lsp-session-folders-blocklist '()  ; Empty blocklist
        lsp-auto-guess-root t  ; Let LSP find project roots automatically

        ;; Additional LSP workspace configuration
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 2000)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/.config/emacs/eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (expand-file-name "~/.config/emacs/workspace/")
        lsp-java-workspace-cache-dir (expand-file-name "~/.config/emacs/workspace/.metadata/.plugins/org.eclipse.core.resources/.projects/")
        lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms1G")
        lsp-java-import-maven-enabled t
        lsp-java-import-gradle-enabled t
        lsp-java-maven-download-sources t
        lsp-java-format-enabled t
        lsp-java-format-on-type-enabled t
        lsp-java-save-actions-organize-imports t
        lsp-java-completion-enabled t
        lsp-java-completion-overwrite t
        lsp-java-completion-guess-method-arguments t))

;; Note: java-spring-boot package removed due to compatibility issues
;; Spring Boot support is provided through lsp-java

;; Debugger
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

  ;; Java debugging
  (require 'dap-java)

  ;; Key bindings for debugging
  :bind
  ("C-c d d" . dap-debug)
  ("C-c d l" . dap-debug-last)
  ("C-c d r" . dap-debug-recent)
  ("C-c d b" . dap-breakpoint-toggle)
  ("C-c d n" . dap-next)
  ("C-c d s" . dap-step-in)
  ("C-c d o" . dap-step-out)
  ("C-c d c" . dap-continue)
  ("C-c d e" . dap-eval-region)
  ("C-c d q" . dap-disconnect))

;;;========================================
;;; AI
;;;========================================

;; for eat terminal backend:
(use-package eat
  :ensure t)

;; for vterm terminal backend:
(use-package vterm
  :ensure t
  :defer t)

;;;; install claude-code.el - no longer using claude code
;; (use-package claude-code
;;   :ensure t
;;   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
;;   :config (claude-code-mode)
;;   :bind-keymap ("C-c c" . claude-code-command-map))


;;; See https://emacs-lsp.github.io/lsp-mode/ for more info
(use-package lsp-pyright
  :ensure t
  :defer t
  :custom
  (lsp-pyright-disable-language-service nil)
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types nil)
  (lsp-pyright-diagnostic-mode "openFilesOnly")
  (lsp-completion-enable t)
  :hook ((python-mode . (lambda ()
			                    (require 'lsp-pyright)
			                    (lsp-deferred)))))

;; Treemacs for project tree view
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (f-join emacs-cache-home "treemacs-persist")
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package ess
  :ensure t
  :defer t)

;; Variable declarations to avoid warnings
(defvar ss/window-move-remap-cookie nil
  "Cookie for face remapping during window moves.")

;; Require necessary packages
(require 'windmove)
(require 'face-remap)

(use-package hydra
  :ensure t
  :init
  (global-set-key (kbd "C-`") 'hydra-windows/body)

  (make-face 'move-window-buffer-face)
  (setq ss/window-move-remap-cookie nil)

  (defun move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))
  (set-face-attribute 'move-window-buffer-face nil
                      :background "#073642")

  (defun remove-window-move-indicator ()
    "Remove the window move visual indicator."
    (when ss/window-move-remap-cookie
      (face-remap-remove-relative ss/window-move-remap-cookie)
      (setq ss/window-move-remap-cookie nil)))

  (defun add-window-move-indicator ()
    (setq
     ss/window-move-remap-cookie
     (face-remap-add-relative 'default 'move-window-buffer-face)))

  (defun window-move (direction)
    "Move to window in DIRECTION and update visual indicator.
DIRECTION can be \\='up, \\='down, \\='left, or \\='right."
    (let ((fun (cond ((eq direction 'up) 'windmove-up)
                     ((eq direction 'down) 'windmove-down)
                     ((eq direction 'left) 'windmove-left)
                     ((eq direction 'right) 'windmove-right))))
      (remove-window-move-indicator)
      (funcall fun)
      (add-window-move-indicator)))

  (defun buffer-swap (direction)
    (let* ((other-window (windmove-find-other-window direction))
           (other-buffer (window-buffer other-window))
           (this-buffer (current-buffer))
           (this-window (selected-window)))
      (set-window-buffer other-window this-buffer)
      (set-window-buffer this-window other-buffer)
      (window-move direction)))

  (defhydra hydra-windows (global-map "C-M-s"
                                      :foreign-keys warn
                                      :pre  add-window-move-indicator
                                      :post remove-window-move-indicator)
    "windows"
    ("<up>" (progn (window-move 'up)))
    ("<down>" (progn (window-move 'down)))
    ("<left>" (progn (window-move 'left)))
    ("<right>" (progn (window-move 'right)))

    ("C-<up>" (progn (buffer-swap 'up)))
    ("C-<down>" (progn (buffer-swap 'down)))
    ("C-<left>" (progn (buffer-swap 'left)))
    ("C-<right>" (progn (buffer-swap 'right)))

    ("w" move-splitter-up)
    ("s" move-splitter-down)
    ("a" move-splitter-left)
    ("d" move-splitter-right)

    ("1" delete-other-windows "max")
    ("2" split-window-below "split below")
    ("-" split-window-below "split below")
    ("3" split-window-right "split right")
    ("|" split-window-right "split right")
    ("+" balance-windows "balance")

    ("C--" (progn (text-scale-increase -0.5)))
    ("C-=" (progn (text-scale-increase 0.5)))

    ("," beginning-of-buffer "home")
    ("." end-of-buffer "end")

    ("f" counsel-find-file)
    ("b" (progn (remove-window-move-indicator)
                (ivy-switch-buffer)
                (add-window-move-indicator)) "switch")
    ("k" (progn (remove-window-move-indicator)
                (kill-this-buffer)
                (add-window-move-indicator)) "kill")
    ("0" (progn (remove-window-move-indicator)
                (delete-window)
                (add-window-move-indicator)) "del")

    ("C-`" nil "exit")
    ("q" nil "exit")))

(use-package diminish
  :ensure t
  :init
  (diminish 'eldoc-mode)
  (diminish 'pcre-mode))

;; ========================================
;; Colors and looks

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;;(set-face-attribute 'mode-line-inactive nil :background "#303030")

  ;;title bar
  (setq frame-title-format "%f (%m) %n")
  (setq ns-use-proxy-icon nil)

  (setq-default cursor-type 'box)
  ;;(setq x-pointer-shape x-pointer-arrow)
  (setq x-stretch-cursor t)

  ;; ========================================
  ;; misc

  ;;(require 'simple-copy)

  ;;global-custom-keys
  (global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
  (global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
  (global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))
  (global-set-key (kbd "<f11>") 'display-line-numbers-mode)

  ;;(global-set-key (kbd "<f1> SPC") 'mark-sexp)

  (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
  (global-set-key [f7] 'toggle-truncate-lines)
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (define-key lisp-interaction-mode-map (kbd "C-x M-e") 'eval-print-last-sexp)

  (global-unset-key (kbd "C-x C-d"))
  (global-unset-key (kbd "<f1> <f1>"))
  (global-unset-key (kbd "C-x <C-left>"))
  (global-unset-key (kbd "C-x <C-right>"))

  (defun yank-without-moving ()
    (interactive)
    (let ((pos (point)))
      (yank)
      (set-window-point nil pos)))

  (global-set-key (kbd "s-y") 'yank-without-moving)

  ;; special chars

  (defun euro ()
    (interactive)
    (insert "€"))

  (defun pound ()
    (interactive)
    (insert "£"))

  (defun usd ()
    (interactive)
    (insert "$"))

  (defun dollar ()
    (interactive)
    (insert "$"))

  ;; camelcase

  (defun un-camelcase-string (s &optional sep start)
    "Convert CamelCase string S to lower case with word separator SEP.
   Default for SEP is a hyphen \"-\".
   If third argument START is non-nil, convert words after that
   index in STRING."
    (let ((done-first nil)
          (case-fold-search nil))
      (while (string-match "[A-ZΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]" s (or start 0))
        (if done-first
            (setq s (replace-match (concat (or sep "-")
                                           (downcase (match-string 0 s)))
                                   t nil s))
          (progn
            (setq s (replace-match (downcase (match-string 0 s)) t nil s))
            (setq done-first 't))))
      (downcase (s-replace "--" "-" s))))

  (defun un-camelcase-region ()
    (interactive)
    (let ((s (buffer-substring (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (un-camelcase-string s))))

  (defun un-camelcase-symbol ()
    (interactive)
    (save-excursion
      (let ((s (format "%s" (symbol-at-point)))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (let ((replacement (un-camelcase-string s)))
          (when replacement
            (delete-region (car bounds) (cdr bounds))
            (insert replacement))))))

  (defun camel->kebab ()
    (interactive)
    (un-camelcase-region))

  ;;

  (defun refresh-file ()
    (interactive)
    (revert-buffer t t t))
  (global-set-key [f5] `refresh-file)
  (global-set-key [f6] `mark-whole-buffer)

  (defun date (arg)
    (interactive "P")
    (insert (if arg
                (format-time-string "%d.%m.%Y")
              (format-time-string "%Y-%m-%d"))))

  (defun new-scratch ()
    "open up a guaranteed new scratch buffer"
    (interactive)
    (switch-to-buffer (cl-loop for num from 0
                               for name = (format "new-%03i" num)
                               while (get-buffer name)
                               finally return name)))

  ;;shell stuff
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  ;;super-slow-scroll
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq scroll-conservatively 10000)

  ;;misc-custom-vars

  ;;global-subword-mode is nice but it adds an annoying comma to modeline
  (global-subword-mode 1)

  (setq frame-resize-pixelwise t)
  (setq inhibit-splash-screen t)
  (setq comment-empty-lines t)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq make-backup-files nil) ;; no backups!
  (setq auto-save-default nil) ;; stop creating those #autosave# files
  (setq custom-file (concat emacs-cache-home "custom.el"))
  (setq temporary-file-directory "/tmp/") ;; necessary for tramp+babel
  ;;(load custom-file 'noerror)
  (column-number-mode t)
  (add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

  (defun my-prog-nuke-trailing-whitespace ()
    (when (derived-mode-p 'prog-mode)
      (delete-trailing-whitespace)))
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (put 'narrow-to-region 'disabled nil)

  ;;spaces-instead-of-tabs
  (setq-default indent-tabs-mode nil)
  (setq-default default-tab-width 2)
  (setq-default tab-width 2)
  (setq-default c-basic-offset 4)
  (setq-default c-indent-level 4)
  (setq-default c++-tab-always-indent nil)
  (setq-default js-indent-level 2)
  (setq-default lua-indent-level 2)
  (setq-default css-indent-offset 2)

  ;;greek support
  (setq default-input-method "greek")
  (global-set-key (kbd "s-\\") 'toggle-input-method)

  ;; ========================================
  ;; Machine-specific config

  (setq auth-sources '("~/.config/emacs/.authinfo.gpg" "~/.config/emacs/.authinfo" "~/.netrc"))

  ;;custom-scratch-message
  (defun slurp (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (setq initial-scratch-message (slurp (concat user-emacs-directory "logo")))

  (defun unix-file ()
    "Change the current buffer to Unix line-ends."
    (interactive)
    (set-buffer-file-coding-system 'unix t))

  ;; see https://github.com/ahungry/org-jira/issues/44
  ;;(setq jiralib-token `("Cookie" . ""))

  (defun google (x)
    (browse-url (concat "https://www.google.com/search?q=" x)))

  (defun google-this ()
    (interactive)
    (if (use-region-p)
        (google (buffer-substring-no-properties (region-beginning) (region-end)))
      (google (word-at-point))))


  (defun jira/insert ()
    "Insert a JIRA link with a description at point"
    (interactive)
    (let* ((id (read-string "Issue ID: "))
           (summary (cdr
                     (assoc 'summary
                            (cdr
                             (assoc 'fields
                                    (car
                                     (org-jira-get-issue-by-id id))))))))
      (insert (format "[%s]: %s" (replace-regexp-in-string "\\-" ":" (downcase id)) summary))))

  (defun ss/copy-file-name ()
    (interactive)
    (kill-new (buffer-file-name)))

  (use-package doom-themes
    :ensure t
    :config
    (require 'doom-themes)
    (if window-system
        (progn
          (load-theme 'doom-one t)
          ;;(load-theme 'doom-vibrant t)
          (scroll-bar-mode -1)
          (setq window-divider-default-right-width 1)
          (set-face-foreground 'vertical-border "#525070")))
    ;;Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

  (defun remove-vowels (string)
    (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" string))

  (defun ss/org-mode-dnd (event &optional new-frame force-text)
    (interactive "e")
    (let* ((window (posn-window (event-start event)))
           (arg (car (cdr (cdr event))))
           (type (car arg))
           (data (car (cdr arg)))
           (buffer (window-buffer window))
           (buffer-dir (file-name-directory (buffer-file-name buffer)))
           (dest-dir (concat buffer-dir "images/"))
           (dest-file (concat dest-dir (file-name-nondirectory data)))
           (rel-file (concat "images/" (file-name-nondirectory data))))

      ;; (if (not (= 'org-mode (with-current-buffer buffer major-mode)))
      ;;     (error "Not an org-mode buffer"))

      (if (not (file-exists-p dest-dir))
          (make-directory dest-dir))

      (rename-file data dest-file t)
      (insert (concat "[[file:" rel-file "]]"))
      (org-display-inline-images)))

  (define-key org-mode-map [drag-n-drop] 'ss/org-mode-dnd)

  (defun ss/json-format (start end)
    (interactive "r")
    (shell-command-on-region start end "jq -r ." nil 't))

  (pixel-scroll-precision-mode)

  ;;(require 'flycheck-joker)

  (defun ss/sql-format (beg end)
    "Beautify SQL in region between beg and END.
  Dependency:
  npm i -g sql-formatter-cli"
    (interactive "r")
    (save-excursion
      (shell-command-on-region beg end "sql-formatter-cli" nil t)))

  (setf epg-pinentry-mode 'loopback) ;; asks for the passphrase in the minibuffer if emacs can't do the decryption
  ;;(setq debug-on-error t)
  (put 'scroll-left 'disabled nil))

(defun ss/truncate (str len)
  "Truncate STR to LEN."
  (if (> (string-width str) len)
      (concat (substring str 0 len) "…")
    str))

(defun ss/org-clock-get-clock-string ()
  "Get clock string."
  (let ((clocked-time (org-clock-get-clocked-time))
        ;; affects performance, too expensive:
        ;; (clocked-time (save-excursion
        ;;                 (set-buffer "time-tracking.org")
        ;;                 (org-clock-sum-today)))
        )
    ;; (propertize (concat (org-duration-from-minutes clocked-time 'h:mm)
    ;;                     ", "
    ;;                     (ss/truncate (format "%s" org-clock-heading) 12))
    ;;             'face 'org-mode-line-clock)
    (propertize (ss/truncate (format "%s" org-clock-heading) 12)
                'face 'org-mode-line-clock)))

(use-package all-the-icons
  :demand t
  :init
  (defun ss/custom-modeline-github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "  %s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1 :family ,(all-the-icons-octicon-family)))
       (propertize " " 'display '(space-width 0.6)) ;;narrow space
       (propertize (format "%s " branch)))))

  (defvar ss/mode-line-my-vc
    '(:propertize
      (:eval (when vc-mode
               (cond
                ((string-match "Git[:-]" vc-mode) (ss/custom-modeline-github-vc))
                (t (format "%s" vc-mode)))))
      face mode-line)
    "Formats the current directory.")
  :config
  (defun justified-mode-line (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT aligned
respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
      (format (format " %%s %%%ds " available-width) left right)))

  (setq-default
   mode-line-format
   '((:eval
      (justified-mode-line
       (format-mode-line
        (list
         ""
         mode-line-mule-info
         mode-line-modified
         mode-line-frame-identification
         mode-line-buffer-identification
         " (%I)"
         " prj:"
         '(:eval (let ((proj (projectile-project-name)))
                   (if (> (string-width proj) 7)
                       (remove-vowels proj)
                     proj)))))
       (format-mode-line
        (list
         mode-line-modes
         ;;'(:eval (symbol-name major-mode))
         '(:eval (when (org-clock-is-active)
                   (concat "  "
                           (propertize (format "%s" (all-the-icons-material "schedule"))
                                       'face `(:family ,(all-the-icons-material-family))
                                       'display '(raise -0.24))
                           (propertize " " 'display '(space-width 0.8)) ;;narrow space
                           (propertize (format "%s" (ss/org-clock-get-clock-string)))
                           " ")))
         ;;ss/mode-line-my-vc
         " ☰ %l ‖ %c ")))))))

(set-face-attribute 'mode-line nil
                    :box '(:line-width 5 :color "#1c1e24")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width 5 :color "#1d2026")
                    :overline nil
                    :underline nil)

(defconst elfeed-feeds
  '("https://news.ycombinator.com/rss"
    "https://grumpygamer.com/rss"
    "https://lobste.rs/rss"
    "https://blog.acolyer.org/feed/"
    "https://www.retronator.com/rss"
    "http://feeds.feedburner.com/stevelosh?format=xml"))

(use-package keycast
  :ensure t
  :defer t)

(use-package crux
  :ensure t
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(use-package ibuffer
  :hook ((ibuffer-mode . (lambda ()
                           (ibuffer-auto-mode 1)
                           (ibuffer-switch-to-saved-filter-groups "groups"))))
  :custom
  (ibuffer-show-empty-filter-groups t)
  (ibuffer-saved-filter-groups
   '(("groups"
      ;;("Notes"      (filename . "\/notes\/*"))
      ("Org"        (mode . org-mode))
      ("Emacs Lisp" (or (mode . emacs-lisp-mode)
                        (mode . elisp-mode)))
      ("Clojure"    (mode . clojure-mode))
      ("Special"    (name . "\*.+\*")))))
  ;; Modify the default ibuffer-formats
  (ibuffer-formats
   '((mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)))
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

(use-package ibuf-ext
  :config
  (add-to-list 'ibuffer-never-show-predicates "magit-process"))

(use-package graphviz-dot-mode
  :ensure t
  :custom (graphviz-dot-dot-program "dot")
  (graphviz-dot-auto-indent-on-semi nil))

(provide 'init)
;;; init.el ends here
