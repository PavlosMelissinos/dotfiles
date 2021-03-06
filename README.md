# dotfiles

dotfiles for the following tools:
* [antigen](https://github.com/zsh-users/antigen)
* [emacs](https://www.gnu.org/software/emacs/)
* [flashfocus](https://github.com/fennerm/flashfocus)
* [gammastep](https://gitlab.com/chinstrap/gammastep)
* [kanshi](https://github.com/emersion/kanshi)
* [mako](https://github.com/emersion/mako)
* [oh-my-tmux](https://github.com/gpakosz/.tmux)
* [oh-my-zsh](https://ohmyz.sh/)
* [swappy](https://github.com/jtheoof/swappy)
* [sway](https://swaywm.org/)
* [swaylock](https://github.com/swaywm/swaylock)
* [waybar](https://github.com/Alexays/Waybar)
* [wofi](https://hg.sr.ht/~scoopta/wofi)

# U2F auth

1. Install required packages:

   `sudo dnf install pamu2fcfg pam-u2f`

1. Generate the key mappings

   `pamu2fcfg > ~/.config/u2f_keys`

   If you have multiple hardware keys, you need to run the following for every
   subsequent key:

   `pamu2fcfg -n >> ~/.config/u2f_keys`

1. Move it so it can be used for centralized authentication:

   `sudo mv ~/'u2f_keys' '/etc/u2f_mappings'`

1. To use it with GDM/GNOME, you need to edit this file:

   `sudo -e '/etc/pam.d/gdm-password'`

   And add the following line underneath the `pam_selinux_permit.so` entry:

   `auth        sufficient    pam_u2f.so authfile=/etc/u2f_mappings cue`

1. And for sudo, it's a similar process; edit the following file:

   `sudo -e '/etc/pam.d/sudo'`

   And add a similar looking line for line 2:

   `auth       sufficient   pam_u2f.so authfile=/etc/u2f_mappings cue`

1. swaylock:

   `sudo -e 'etc/pam.d/swaylock'`

   and add the following line:

   `auth sufficient pam_u2f.so authfile=/etc/u2f_mappings cue`

   Press the key button and then enter to log-in

U2F auth setup instructions are heavily based on [this
article](https://reddit.com/r/Fedora/comments/akck9m/authenticating_with_gdm_and_sudo_with_a_u2f/)
and adapted from [the Arch
guide](https://wiki.archlinux.org/index.php/Universal_2nd_Factor#Adding_a_key).
