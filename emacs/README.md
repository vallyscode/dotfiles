Nerd fonts:
https://nerdfonts.com/#downloads

Linux fonts

Can be placed in: /usr/share/fonts or ~/.fonts
After placing fonts: sudo fc-cache

Usafull commands:
fc-list – lists fonts
fc-match -s helvetica – show an ordered list of fonts matching a certain name or pattern
fc-cache -fv – rebuilds cached list of fonts (in ~/.config/fontconfig, older caches may also be in ~/.fontconfig) 

Example:
vagrant@devbox:~/.fonts$ fc-match -s Terminus | grep Terminus
Terminus (TTF) Nerd Font Complete Mono Windows Compatible.ttf: "TerminusTTF NF" "Medium"

OSX

Can be placed in: ~/Library/Fonts/ or /Library/Fonts/
https://support.apple.com/en-us/HT201722
https://support.apple.com/en-us/HT201749

