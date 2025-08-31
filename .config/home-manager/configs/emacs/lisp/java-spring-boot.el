;;; java-spring-boot.el --- Some configuration for Java Spring Boot -*- lexical-binding: t -*-

;; Created: Fang lungang 11/14/2010
;; Modified: Fang Lungang 11/13/2015 10:09>

;; Copyright (C) 2010  Fang lungang

;;; Commentary:
;;; Code:

(defvar c-basic-offset)
(defvar c-hanging-braces-alist)
(defvar c-offsets-alist)
(defvar java-mode-map)
(defvar lsp-log-io)
(defvar lsp-trace)
(defvar lsp-print-performance)
(defvar lsp-auto-guess-root)
(defvar lsp-document-sync-method)
(defvar lsp-response-timeout)
(defvar lsp-idle-delay)
(defvar lsp-enable-file-watchers)

(add-hook 'java-mode-hook
          (lambda ()
            ;; Indentation
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil
                  c-hanging-braces-alist '((brace-list-open)
                                           (brace-entry-open)
                                           (statement-cont)
                                           (substatement-open after)
                                           (block-close . c-snug-do-while)
                                           (extern-lang-open after)
                                           (namespace-open after)
                                           (module-open after)
                                           (composition-open after)
                                           (inexpr-class-open after)
                                           (inexpr-class-close before)
                                           (arglist-cont-nonempty)))

            ;; Code formatting
            (setq c-offsets-alist '((arglist-intro . +)
                                    (arglist-cont . 0)
                                    (arglist-cont-nonempty . +)
                                    (arglist-close . 0)
                                    (case-label . +)
                                    (access-label . -)
                                    (inclass . +)
                                    (inline-open . 0)
                                    (statement-cont . +)))

            ;; Enable subword mode for camelCase navigation
            (subword-mode 1)

            ;; Auto-fill comments
            (auto-fill-mode 1)
            (setq fill-column 100)

            ;; Show line numbers
            (display-line-numbers-mode 1)

            ;; Highlight current line
            (hl-line-mode 1)))

;; Custom key bindings for Java development
(define-key java-mode-map (kbd "C-c C-v") 'lsp-java-extract-to-variable)
(define-key java-mode-map (kbd "C-c C-m") 'lsp-java-extract-to-method)
(define-key java-mode-map (kbd "C-c C-r") 'lsp-rename)
(define-key java-mode-map (kbd "C-c C-o") 'lsp-java-organize-imports)
(define-key java-mode-map (kbd "C-c C-f") 'lsp-format-buffer)
(define-key java-mode-map (kbd "C-c C-a") 'lsp-execute-code-action)
(define-key java-mode-map (kbd "C-c C-t") 'maven-test-toggle-between-test-and-code)

;;; Utility functions

;; Function to create a new Java class
(defun create-java-class (class-name)
  "Create a new Java class, CLASS-NAME, with basic template."
  (interactive "sClass name: ")
  (let ((package-name (read-string "Package name: "))
        (file-name (concat class-name ".java")))
    (find-file file-name)
    (insert (format "package %s;\n\n" package-name))
    (insert (format "public class %s {\n\n" class-name))
    (insert "    public static void main(String[] args) {\n")
    (insert "        // TODO: Implement\n")
    (insert "    }\n")
    (insert "}\n")
    (goto-char (point-min))
    (search-forward "TODO")))

;; Function to create Spring Boot controller
(defun create-spring-controller (controller-name)
  "Create a new Spring Boot controller CONTROLLER-NAME."
  (interactive "sController name: ")
  (let ((package-name (read-string "Package name: "))
        (file-name (concat controller-name "Controller.java")))
    (find-file file-name)
    (insert (format "package %s;\n\n" package-name))
    (insert "import org.springframework.web.bind.annotation.*;\n")
    (insert "import org.springframework.http.ResponseEntity;\n\n")
    (insert "@RestController\n")
    (insert (format "@RequestMapping(\"/api/%s\")\n" (downcase controller-name)))
    (insert (format "public class %sController {\n\n" controller-name))
    (insert "    @GetMapping\n")
    (insert "    public ResponseEntity<String> hello() {\n")
    (insert "        return ResponseEntity.ok(\"Hello from " controller-name "!\");\n")
    (insert "    }\n")
    (insert "}\n")))

;; Function to run Maven commands
(defun run-maven-command (command)
  "Run a Maven COMMAND in the project root."
  (interactive "sMaven command: ")
  (let ((project-root (when (fboundp 'projectile-project-root)
                        (projectile-project-root))))
    (if project-root
        (let ((default-directory project-root))
          (compile (concat "mvn " command)))
      (message "Not in a Maven project"))))

;; Quick Maven commands
(defun maven-clean-compile ()
  "Run \='mvn clean compile\='."
  (interactive)
  (run-maven-command "clean compile"))

(defun maven-test ()
  "Run \='mvn test\='."
  (interactive)
  (run-maven-command "test"))

(defun maven-spring-boot-run ()
  "Run \='mvn spring-boot:run\='."
  (interactive)
  (run-maven-command "spring-boot:run"))

;; Spring Boot support
(defun spring-initializr (project-name)
  "Create a new Spring Boot project, PROJECT-NAME, using start.spring.io."
  (interactive "sProject name: ")
  (let ((url (concat "https://start.spring.io/starter.zip"
                     "?type=maven-project"
                     "&language=java"
                     "&bootVersion=3.2.0"
                     "&baseDir=" project-name
                     "&groupId=com.example"
                     "&artifactId=" project-name
                     "&name=" project-name
                     "&description=Demo%20project%20for%20Spring%20Boot"
                     "&packageName=com.example." project-name
                     "&packaging=jar"
                     "&javaVersion=21"
                     "&dependencies=web,data-jpa,h2")))
    (browse-url url)))


;; Global key bindings for Java utilities
(global-set-key (kbd "C-c j c") 'create-java-class)
(global-set-key (kbd "C-c j s") 'create-spring-controller)
(global-set-key (kbd "C-c j m c") 'maven-clean-compile)
(global-set-key (kbd "C-c j m t") 'maven-test)
(global-set-key (kbd "C-c j m r") 'maven-spring-boot-run)


;;; Performance optimizations

;; Increase garbage collection threshold
(setq gc-cons-threshold 100000000)

;; Increase read process output max
(setq read-process-output-max (* 1024 1024))

;; LSP performance tweaks
(with-eval-after-load 'lsp-mode
  (setq lsp-log-io nil
        lsp-trace nil
        lsp-print-performance nil
        lsp-auto-guess-root nil
        lsp-document-sync-method 'incremental
        lsp-response-timeout 10
        lsp-idle-delay 0.5
        lsp-enable-file-watchers nil))

;;; Theme and UI enhancements (optional)

;; ;; Install a modern theme
;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config))
;;
;; ;; Mode line enhancement
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 25
;;         doom-modeline-bar-width 3
;;         doom-modeline-project-detection 'projectile
;;         doom-modeline-buffer-file-name-style 'truncate-except-project
;;         doom-modeline-major-mode-icon t))
;;
;; ;; Icons for better visual experience
;; (use-package all-the-icons)

;;; Final message
(message "Java development environment loaded successfully!")
(message "Key bindings:")
(message "  C-c l    - LSP commands")
(message "  C-c d    - Debug commands")
(message "  C-c j    - Java utilities")
(message "  C-c p    - Projectile commands")
(message "  C-x g    - Magit status")
(message "  M-0      - Treemacs toggle")

(provide 'java-spring-boot)
;;; java-spring-boot.el ends here
