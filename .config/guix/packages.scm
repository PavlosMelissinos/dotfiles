;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(specifications->manifest
  (list "thunar"
        ;; System packages that are difficult to replace
        "qtbase"
        "geoclue"
        "docker-compose"
        "gnome-control-center"
        "motion"
        
        ;; Emacs packages (specialized Guix versions)
        "emacs-next-pgtk"
        "emacs-all-the-icons"
        "emacs-geiser-guile"
        "emacs-geiser"
        
        ;; Audio control
        "pavucontrol"
        
        ;; Utilities not easily replaced
        "bsd-games"
        "mc"
        "xeyes"
        "iftop"
        "pv"
        "lilypond"
        
        ;; System libraries and fonts (keep for system integration)
        "fontconfig"
        "nss-certs"
        "font-gnu-freefont"
        "glibc-locales"
        "pth"
        "font-dejavu"
        "font-liberation"
        "font-awesome"
        "font-ghostscript"
        "glibc"))
