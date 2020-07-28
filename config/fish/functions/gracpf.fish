function gracpf -d "git rebase, add, commit, push -f" --argument-names 'message'
    git fetch upstream
    git reset upstream/master
    git add .
    git commit -m "$message"
    git push -f
end
