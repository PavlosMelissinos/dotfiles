;; init.el --- My Emacs configuration
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

;; ENV VARIABLES
(defun safe-getenv (env)
  "Provides a safe way to retrieve environment variables from ENV.

Examples TODO."
  (if (bound-and-true-p exec-path-from-shell-getenv)
    (exec-path-from-shell-getenv env)
    (getenv env)))

(defconst emacs-config-home user-emacs-directory)
(defconst emacs-cache-home (expand-file-name "emacs/" (or (safe-getenv "XDG_CACHE_HOME") "~/.cache")))
(defconst emacs-data-home (expand-file-name "emacs/" (or (safe-getenv "XDG_DATA_HOME") "~/.local/share")))
(defconst emacs-state-home (expand-file-name "emacs/" (or (safe-getenv "XDG_STATE_HOME") "~/.local/state")))

;; other directories
(setq package-user-dir (concat emacs-cache-home "elpa")) ;; do not litter user-emacs-directory with elpa cache
;; ========================================
;; package

;;(setq debug-on-error 't)

(defun init ()
  "Edit your init file."
  (interactive)
  (find-file (concat emacs-config-home "init.el")))

(setq package-archives
      '(("GNU ELPA"  . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")

        ;;("nongnu" . "https://elpa.nongnu.org/nongnu/")
        )
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA"        . 5)
        ("MELPA Stable" . 0)))

;; Initialize the packages, avoiding a re-initialization.

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Make sure `use-package' is available.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ========================================
;; Modes

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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


(use-package neotree
  :bind (:map neotree-mode-map
         ("^" . neotree-select-up-node))
  :ensure t
  :config
  (define-key neotree-mode-map "^" 'neotree-select-up-node))

(use-package emacs-lisp-mode
  :no-require t
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "elisp")))))

(use-package clojure-snippets
  :ensure t)

(defvar clojure-mode-map (make-keymap))

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

  (defun toggle-test-path (path)
    (ss/string-join
     "/"
     (mapcar
      (lambda (x)
        (cond ((string-equal x "test") "src")
              ((string-equal x "src") "test")

              ((string-equal x "src-cljs") "test-cljs")
              ((string-equal x "test-cljs") "src-cljs")

              ((string-match "\\(.+\\)_test\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) ".clj" (match-string 2 x)))
              ((string-match "\\(.+\\)\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) "_test.clj" (match-string 2 x)))

              (t x)))
      (split-string path "/"))))

  (defun clojure-jump-to-test ()
    "Jump from a source buffer to its corresponding test buffer and vice versa."
    (interactive)
    (find-file (toggle-test-path buffer-file-name)))

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
         ("<C-M-up>" . transpose-sexp-backward)
         ("<C-M-down>" . transpose-sexp-forward)
         ("<M-S-left>" . backward-sexp)
         ("<M-S-right>" . forward-sexp))
  :init
  (defun duplicate-sexp ()
    "Duplicates the sexp at point."
    (interactive)
    (save-excursion
      (forward-sexp)
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (insert (concat (buffer-substring (car bounds) (cdr bounds)) "\n"))
        (indent-for-tab-command))))

  (defun transpose-sexp-forward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps 1)
    (backward-sexp))

  (defun transpose-sexp-backward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps -1)
    (backward-sexp)))

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
         ;;("<f12>" . apply-fix-macro)
         )
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)

  (defun macroexpand-replace ()
    (interactive)
    (let ((exp
           (cider-sync-request:macroexpand
            "macroexpand-1"
            (cider-last-sexp))))
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (delete-region (car bounds) (cdr bounds))
        (insert exp)
        (indent-for-tab-command))))

  (defun apply-fix-macro ()
    (interactive)
    (paredit-wrap-round)
    (insert "fix ")
    (forward-sexp)
    (forward-char 1)
    (macroexpand-replace)
    (backward-sexp))

  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-history-file (concat emacs-data-home "cider-history"))
  (cider-font-lock-dynamically nil)
  (cider-repl-wrap-history t)
  (cider-repl-history-size 3000)
  (cider-repl-buffer-size-limit 100000)
  (cider-show-error-buffer 'except-in-repl)
  (cider-repl-display-help-banner nil)
  (cider-inject-dependencies-at-jack-in t)
  (nrepl-prompt-to-kill-server-buffer-on-quit nil)
  (cider-repl-result-prefix ";; => ")

  ;; Try to replicate this workflow: https://github.com/clojure-emacs/cider/issues/2617
  (cider-invert-insert-eval-p t)
  (cider-repl-pop-to-buffer-on-connect nil)

  (clojure-quick-sexp
   '("(dev/reset)" "(user/fix)" "(use 'clojure.repl)"
     "(use 'clojure.tools.trace)" "(use 'clojure.pprint)"
     "(dev/start-cljs-figwheel)"))

  :config
  (set-face-attribute 'cider-test-failure-face nil :background "#8c2020")

  (defun replace-not-in-strings (start end match replacement)
    "Only tested on single characters"
    (set-mark nil)
    (let ((p (point))
          (pos start))
      ;;(setq pos start)
      (while (< pos end)
        (goto-char pos)
        (let ((faces (face-at-point t t)))
          (princ faces)
          (princ "\n")
          (cond ((member 'font-lock-string-face faces)
                 (princ "case 1\n")
                 (setq pos (1+ pos)))

                ((string-equal match (buffer-substring pos (1+ pos)))
                 (princ "case 2\n")
                 (delete-char 1)
                 (insert replacement)
                 (setq pos (1+ pos)))

                (:else (setq pos (1+ pos))))))
      (goto-char p))))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :no-require t
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
  (projectile-mode nil))

(use-package terraform-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-load-directory (concat user-emacs-directory "snippets")))

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
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; Don't wait before completion.
  (company-idle-delay 0)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)
         (python-mode . company-mode))
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
         ("C-c s" . ss/standup))
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
                                 (js         . t)
                                 (perl       . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (shell      . t)
                                 (sql        . t)))

  ;; (defun sql-to-org-table ()
  ;;   (interactive)
  ;;   (mc/edit-lines)
  ;;   (org-force-self-insert "|")
  ;;   (multiple-cursors-mode))

  (defun ss/standup (start end)
    (interactive "r")
    (let ((oldbuf (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring oldbuf start end)
        (goto-char (point-min))
        (replace-string " BLOK" " :todo-pause:")
        (goto-char (point-min))
        (replace-string " DONE" " :todo-done:")
        (goto-char (point-min))
        (replace-string " PROG" " :todo-doing:")
        (goto-char (point-min))
        (replace-string " CNCL" " :todo-cancel:")
        (goto-char (point-min))
        (replace-string " TODO" " :todo:")
        (goto-char (point-min))
        (replace-regexp "^\\* " "") ; remove header stars and space
        (goto-char (point-min))
        (replace-regexp "^\\*" "") ; remove item stars
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


(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets


(use-package org-tree-slide
  :ensure t
  ;; :bind (("<f12>" . org-tree-slide-mode)
	;;  ("<S-f12>" . org-tree-slide-skip-done-toggle))
  :init (org-tree-slide-simple-profile))

(use-package org-roam
  :ensure t
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
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-superstar
  :ensure t
  :custom (org-bullets-bullet-list '("●"))
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-jira
  :defer t
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
  :defer t
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
  (global-set-key (kbd "C-x g") 'magit-status)

  (defun ss/current-line ()
    (let ((start (point-min))
	        (n (line-number-at-pos)))
      (if (= start 1)
	        n
        (save-excursion
	        (save-restriction
	          (widen)
	          (+ n (line-number-at-pos start) -1))))))

  (defun ss/magit-find-file (rev file)
    (interactive (magit-find-file-read-args "Find file"))
    (let ((line (ss/current-line)))
      (magit-find-file rev (magit-current-file))
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter-top-bottom)))

  ;; also see: git log -n 1 --pretty=format:%H -- my/file.c
  (defun ss/prev-magit-find-file ()
    (interactive)
    (let ((rev (if (not magit-buffer-refname)
                   (car (magit-commit-parents (magit-rev-parse-safe "--branches")))
                 (car (magit-commit-parents magit-buffer-refname))))
          (line (ss/current-line)))
      (if rev
          (progn
            (magit-find-file rev (magit-current-file))
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)
            (message (format "Switched to %s." magit-buffer-refname)))
        (message "Current file rev cannot be determined")))))

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
                   :background nil
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
  ;;:bind (("<^>" . (lambda () (find-alternate-file "..")))) ;;TODO
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

(use-package display-line-numbers
  :init
  (global-set-key (kbd "<f11>") 'display-line-numbers-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :ensure t
  ;;:no-require t
  ;; :hook ((lisp-mode . highlight-symbol-mode)
  ;;        (emacs-lisp-mode . highlight-symbol-mode)
  ;;        (scheme-mode . highlight-symbol-mode)
  ;;        (cider-repl-mode . highlight-symbol-mode)
  ;;        (clojure-mode . highlight-symbol-mode)
  ;;        (python-mode . highlight-symbol-mode))
  :init
  (global-set-key (kbd "C-,") 'highlight-symbol-prev)
  (global-set-key (kbd "C-.") 'highlight-symbol-next)
  :config
  (setq highlight-symbol-mode t)
  (setq highlight-symbol-idle-delay 1)
  (setq highlight-symbol-on-navigation-p 't)
  (setq highlight-symbol-occurrence-message (quote (explicit)))
  (custom-set-faces
   '(highlight-symbol-face ((t (:foreground "gray100" :background "#9c7618" :weight semi-bold)))))

  (add-hook 'find-file-hook 'my-hls-hook)

  (defun highlight-symbol-count (&optional symbol)
    "(Do not) Print the number of occurrences of symbol at point."
    (interactive))

  (defun my-hls-hook ()
    (when (featurep 'highlight-symbol)
      (highlight-symbol-mode t)
      (highlight-symbol-nav-mode t))))

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
  :custom (browse-at-remote-add-line-number-if-no-region-selected t))

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
                          (setq mode-name "Py"))))
  ;; Remove guess indent python message
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython"
	          python-shell-interpreter "ipython"
	          python-shell-interpreter-args "-i --simple-prompt")))
   ((setq python-shell-interpreter "python")))

  (setq elpy-shell-echo-output t ; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-574512892
   )
  )

;; Hide the modeline for inferior python processes (as well as R)
(use-package inferior-python-mode
  :ensure nil
  :hook ((inferior-python-mode . hide-mode-line-mode)
	       (inferior-ess-r-mode . hide-mode-line-mode)))

(use-package hide-mode-line
  :ensure t
  :defer t)

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
(use-package yapfify
  :diminish yapf-mode
  :ensure t
  :defer t
  ;;:hook (python-mode . yapf-mode)
  )

;; numpy docstring for python
(use-package numpydoc
  :ensure t
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;;; `lsp-mode` proper
;;; The config should be relatively agnostic up to here

(use-package lsp-mode
  :ensure t
  :defer t
  :delight " LSP"
  :defines (lsp-keymap-prefix lsp-mode-map)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate nil)
  (lsp-completion-enable t)
  (gc-cons-threshold 1600000)
  (read-process-output-max (* 1024 1024))
  :hook ((python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
	      ("M-<RET>" . lsp-execute-code-action)))

;; Debugger
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;;; See https://emacs-lsp.github.io/lsp-mode/ for more info
;;; Dependency:
;;;  npm i -g pyright
(use-package lsp-pyright
  :ensure t
  :defer t
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3"))
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

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (evil-define-key '(motion normal) 'local (kbd "K")
                                     #'(lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line)))))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enhanced-markdown nil)
  (setq lsp-ui-doc-delay 0.01)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-text-scale-level -1.0)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-max-height 25)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (setq lsp-ui-sideline-diagnostic-max-line-length 80)
  (setq lsp-ui-sideline-diagnostic-max-lines 2)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-java
  :after lsp)

(use-package java
  :ensure nil
  :after lsp-java
  :bind (:map java-mode-map ("C-c i" . lsp-java-add-import)))

(use-package repl-driven-development
  :ensure t
  :config
  (repl-driven-development [C-x C-j] java)       ;; e“X”ecute “j”ava
  (repl-driven-development [C-x C-n] javascript) ;; e“X”ecute “n”odejs
  ;;(repl-driven-development [C-x C-p] python)     ;; e“X”ecute “p”ython
  (repl-driven-development [C-x C-t] terminal))  ;; e“X”ecute “t”erminal

(use-package ess
  :ensure t
  :defer t)

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
    (if ss/window-move-remap-cookie
        (face-remap-remove-relative
         ss/window-move-remap-cookie)))

  (defun add-window-move-indicator ()
    (setq
     ss/window-move-remap-cookie
     (face-remap-add-relative 'default 'move-window-buffer-face)))

  (defun window-move (direction)
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
  (setenv "GPG_AGENT_INFO" nil)

  ;; ========================================
  ;; misc

  ;;(require 'simple-copy)

  ;;global-custom-keys
  (global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
  (global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
  (global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

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
  (setq-default c-indent-level 4)
  (setq-default c++-tab-always-indent nil)
  (setq-default js-indent-level 4)
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
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
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

  (pixel-scroll-mode)
  (defvar pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
  (defvar pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
  (setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
  (setq mouse-wheel-progressive-speed nil)

  ;;(require 'flycheck-joker)

  (defun ss/sql-format (beg end)
    "Beautify SQL in region between beg and END.
  Dependency:
  npm i -g sql-formatter-cli"
    (interactive "r")
    (save-excursion
      (shell-command-on-region beg end "sql-formatter-cli" nil t)))

  ;;(setq debug-on-error t)
  (put 'scroll-left 'disabled nil))

(add-to-list 'load-path (concat emacs-config-home "lisp/copilot.el"))

(use-package copilot
  ;; :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  ;; :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-mode-map (kbd "C-<tab>") 'copilot-accept-completion)
  ;;(define-key copilot-mode-map "M-Y" 'copilot-accept-completion)
  ;;(define-key copilot-mode-map "M-y" 'copilot-accept-completion-by-line)
  (setq copilot-enable-predicates '(evil-insert-state-p copilot--buffer-changed))
  )

(use-package copilot-chat
  :config
  (define-key copilot-mode-map (kbd "C-c c e") 'copilot-chat-explain)
  (define-key copilot-mode-map (kbd "C-c c r") 'copilot-chat-review)    ; Refactor region
  (define-key copilot-mode-map (kbd "C-c c f") 'copilot-chat-fix)       ; Fix code in region
  (define-key copilot-mode-map (kbd "C-c c c p") 'copilot-chat-custom-prompt-function)
  (define-key copilot-mode-map (kbd "C-c c d") 'copilot-chat-doc)  ; Add documentation
  )

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
  :init
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
