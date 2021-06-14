;; This channel file can be passed to 'guix pull -C' or to
;; 'guix time-machine -C' to obtain the Guix revision that was
;; used to populate this profile.

(list
     (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit
         "732da2201eb150c968990bd731a5a282247f6511")
       (introduction
         (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
     (channel
       (name 'flat)
       (url "https://github.com/flatwhatson/guix-channel.git")
       (commit
         "86fb7253a4384b70c77739a0e03115be75d60ad1")
       (introduction
         (make-channel-introduction
           "33f86a4b48205c0dc19d7c036c85393f0766f806"
           (openpgp-fingerprint
             "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
     (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (commit
         "d3c5eea0cbfe3e5bfbcf1fe15bc916fefacc624f"))
)
