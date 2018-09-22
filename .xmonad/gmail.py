#!/usr/bin/env python3

import imaplib
import sys

mail = imaplib.IMAP4_SSL('imap.gmail.com', 993)
mail.login(sys.argv[1], sys.argv[2])
mail.select('inbox')

typ, messageIds = mail.search(None, 'UNSEEN')
ms = str( messageIds[0], encoding='utf8' )
lst = ms.split(" ")

if lst == ['']:
    print(0)
else:
    print(len(lst))
