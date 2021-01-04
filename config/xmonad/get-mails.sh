#!/run/current-system/sw/bin/bash

notmuch new --quiet
res=$(notmuch count tag:unread folder:'gmail/Inbox')
res2=$(notmuch count tag:unread folder:'hasura/Inbox')

echo "G$res:H$res2"

exit 0
