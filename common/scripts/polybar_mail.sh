#!/bin/sh
propagate_deletions()
{
	  local COUNT=`notmuch count tag:deleted`
	  test "$COUNT" = 0 && return
	  echo "- deleting $COUNT messages ..."
	  notmuch search --format=text0 --output=files tag:deleted | xargs -0 --no-run-if-empty rm
}

STATE=`/run/current-system/sw/bin/nmcli networking connectivity`

if [ $STATE = 'full' ]
then
  echo check mail > ~/.mail.log
  (~/dotfiles/common/scripts/msmtpqueue/msmtp-runqueue.sh > /dev/null 2>&1 )&
  (/run/current-system/sw/bin/mbsync -aqq > /dev/null 2>&1 )&
  (/run/current-system/sw/bin/notmuch new > /dev/null 2>&1 )&
  (/run/current-system/sw/bin/afew -tn > /dev/null 2>&1 )&
  (/run/current-system/sw/bin/notmuch tag -inbox tag:inbox AND tag:lists > /dev/null 2>&1 )&
  propagate_deletions
fi

shopt -s nullglob
numfiles=(~/.mail/oyra_mscheuren/Inbox/new/*)
numfiles=${#numfiles[@]}
if [ $numfiles = "0" ]
then
	echo  $numfiles
else
	echo  $numfiles
	#aplay ~/dotfiles/common/scripts/new_notification.wav
fi



