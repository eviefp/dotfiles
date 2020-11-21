#!/run/current-system/sw/bin/bash

notmuch new --quiet
res=$(notmuch count tag:unread folder:'gmail/Inbox')

echo $vol

exit 0
