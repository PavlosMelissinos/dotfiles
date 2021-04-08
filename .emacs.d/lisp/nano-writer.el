;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(require 'org)
(require 'nano-base-colors)
(require 'nano-faces)

;;
;;             prefix
;;           |<------>|

;; border -> |<------>| * Headline level 1  # Unnumbered
;;           |<------>| 1 Headline level 1  # Numbered
;;
;;           |<----->| ** Headline level 2  # Unnumbered
;;           |<---->| 1.1 Headline level 2  # Numbered
;;
;;           |<---->| *** Headline level 3  # Unumbered
;;           |<-->| 1.1.1 Headline level 3  # Numbered
;; etc.
;;
;; This works if the number of sections at a given level is < 10.


(defun writer-mode--num-format (numbering)
  "Alternative numbering format for org-num.

First level: 1 | xxx
Second level: 1.1 — xxx
Third level: 1.1.1 - xxx
etc.
"""
  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "Roboto Condensed"
                          :height 250
                          :foreground ,nano-color-faded))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "Roboto Condensed"
                        :foreground ,nano-color-faded))))

;; Specific face for headline stars
(font-lock-add-keywords 'writer-mode
             '(("^*+ " 0 `(:family "Roboto Mono"
                           :height 140
                           :foreground ,nano-color-faded) prepend)
               ) 'append)

(defun writer-mode--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."

  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (let* ((min-indent 5)
         (indent (+ 1 (seq-max
                  (org-element-map
                      (org-element-parse-buffer) 'headline
                    #'(lambda (item)
                        (org-element-property :level item))))))
         (indent (max indent min-indent)))

  (dotimes (n org-indent--deepest-level)
    (aset org-indent--heading-line-prefixes n
          (make-string
           (min indent (max 0 (- indent 1 n))) ?\s))
    (aset org-indent--inlinetask-line-prefixes n
          (make-string indent ?\s))
    (aset org-indent--text-line-prefixes n
          (make-string indent ?\s)))))



(define-derived-mode writer-mode org-mode "NΛNO writer"

  ;; Faces
  (face-remap-add-relative 'org-level-1
                           :overline nano-color-subtle
                           :family "Roboto" :height 2.0
                           )
  (face-remap-add-relative 'org-level-2
                           :family "Roboto" :height 1.8)
  (face-remap-add-relative 'org-level-3
                           :family "Roboto" :height 1.5)
  (face-remap-add-relative 'org-document-info
                           :inherit 'nano-face-faded)
  (face-remap-add-relative 'org-document-title
                           :foreground nano-color-foreground
                           :family "Roboto Slab"
                           :height 2.5
                           :weight 'medium)
  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date startup))

  ;; turn progress text into symbols
  (font-lock-add-keywords
   'writer-mode `(("^\\*+ \\(TODO\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "□") nil)))
               ("^\\*+ \\(PROG\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "▶") nil)))
               ("^\\*+ \\(BLOK\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘") nil)))
               ("^\\*+ \\(DONE\\) " (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔") nil)))))

  ;; Header line
  (setq header-line-format nil)

  ;; Layout
  (setq fill-column 72)
  (setq-default line-spacing 1)

  ;; Indentation
  (setq org-startup-folded nil)
  (org-indent-mode)
  (setq org-level-color-stars-only nil)
  (setq org-hide-leading-stars t)
  (advice-add 'org-indent--compute-prefixes :override
              #'writer-mode--compute-prefixes)

  ;; Numbering
  (setq org-num-skip-unnumbered t)
  (setq org-num-skip-footnotes t)
  (setq org-num-max-level 3)
  (setq org-num-face nil)
  (org-num-mode)
  (setq org-num-format-function 'writer-mode--num-format))

(provide 'nano-writer)
