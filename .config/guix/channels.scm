;; This channel file can be passed to 'guix pull -C' or to
;; 'guix time-machine -C' to obtain the Guix revision that was
;; used to populate this profile.

(list
     (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit
        "52269e27c628d34af896e95347870b1755cb3e5f")
       (introduction
         (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
     (channel
      (name 'flat)
      (url "https://github.com/flatwhatson/guix-channel.git")
      (commit
       "ece373bcd1e00ef746fb7b104e56ab610cc4af2a")
      (introduction
       (make-channel-introduction
        "33f86a4b48205c0dc19d7c036c85393f0766f806"
        (openpgp-fingerprint
         "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
     (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit
         "35533c763a81365e14d0b58ad262e8f88c82872c")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
)
