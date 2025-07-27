;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;; Empty manifest - all packages now managed by home-manager/Nix
;; This effectively disables Guix package management while keeping 
;; the daemon available for system integration if needed
(specifications->manifest (list))
