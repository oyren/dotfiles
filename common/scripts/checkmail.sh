#!/bin/sh

STATE=`/run/current-system/sw/bin/nmcli networking connectivity`

if [ $STATE = 'full' ]
then
    ~/dotfiles/common/scripts/msmtpqueue/msmtp-runqueue.sh
    /run/current-system/sw/bin/mbsync -aqq
    /run/current-system/sw/bin/notmuch new
    /run/current-system/sw/bin/afew -tn
    /run/current-system/sw/bin/notmuch tag -inbox tag:inbox AND tag:lists
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
