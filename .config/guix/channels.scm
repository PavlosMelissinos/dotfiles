;; This channel file can be passed to 'guix pull -C' or to
;; 'guix time-machine -C' to obtain the Guix revision that was
;; used to populate this profile.

(list
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  ;; (commit
  ;;  "31c9f0e30108bc9845df85e9b3f440d53b002409")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  ;; (branch "master")
  ;; (commit
  ;;  "7f2b80515ff52efae16a1c9346e2778b0d59d03b")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'guix-gaming-games)
  (url "https://gitlab.com/guix-gaming-channels/games.git")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
    (openpgp-fingerprint
     "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
 )
