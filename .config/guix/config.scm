;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop networking ssh sddm)
(use-package-modules bootloaders emacs wm)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Athens")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "tim0")
  (users (cons* (user-account
                  (name "pavlos")
                  (comment "Pavlos")
                  (group "users")
                  (home-directory "/home/pavlos")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "awesome")
            ;; (specification->package "i3-wm")
            ;; (specification->package "i3status")
            ;; (specification->package "dmenu")
            (specification->package "sway")
            (specification->package "waybar")
            (specification->package "wofi")
            (specification->package "st")
            (specification->package "nss-certs"))
      %base-packages))
  (services
    ;; (append
    ;;   (list (service openssh-service-type)
    ;;         (service sddm-service-type
    ;;                  (sddm-configuration
    ;;                   (display-server "wayland")
    ;;                   (auto-login-session "sway.desktop")))
    ;;         (set-xorg-configuration
    ;;          (xorg-configuration
    ;;           (keyboard-layout keyboard-layout))))
    ;;   %desktop-services)
   (append
    %desktop-services
    (list (service openssh-service-type)
          (service sddm-service-type
                   (sddm-configuration
                    (display-server "wayland")
                    (auto-login-session "sway.desktop")))))
   (append
    (cons* (service openssh-service-type)
           (service sddm-service-type
                    (sddm-configuration
                     (display-server "wayland"))))))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "f023d40c-db25-487b-a3b3-73f74cffa722")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
              (uuid "d2d423c7-659e-4fc5-afdd-5c2437f35c45"
                     'ext4))
             (type "ext4"))
           ;; (file-system
           ;;  (mount-point "/home")
           ;;  (type "ext4")
           ;;  (device (uuid "4dab5feb-d176-45de-b287-9b0a6e4c01cb")))
           %base-file-systems)))
