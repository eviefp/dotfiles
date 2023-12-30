(setq org-agenda-files
      (quote ("~/code/personal-org/agenda/todo.org"
              "~/code/personal-org/agenda/backlog.org"
              "~/code/personal-org/cal-sync/evie.org"
              "~/code/personal-org/cal-sync/gia-evie.org"
              "~/code/personal-org/cal-sync/proton.org"
              "~/code/personal-org/agenda/todo-history.org")))

(custom-set-variables
 '(org-agenda-ndays 30)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t))
