#!/bin/sh

STATE=`~/.nix-profile/bin/nmcli networking connectivity`

if [ $STATE = 'full' ]
then
    ~/dotfiles/common/scripts/msmtpqueue/msmtp-runqueue.sh
    ~/.nix-profile/bin/mbsync -aqq
    ~/.nix-profile/bin/notmuch new
    ~/.nix-profile/bin/afew -tn
    ~/.nix-profile/bin/notmuch tag -inbox tag:inbox AND tag:lists
    exit 0

fi
echo "No Internet!"

#nc -z 8.8.8.8 53  >/dev/null 2>&1
#online=$?
#if [ $online -eq 0 ]; then
#    echo "Online"
#	~/dotfiles/common/scripts/msmtpqueue/msmtp-runqueue.sh
#	mbsync -aqq
#	notmuch new
#	afew -tn
#	notmuch tag -inbox tag:inbox AND tag:lists
#else
#    echo "Offline"
#fi
exit 0
