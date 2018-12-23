function get_file_age -a file -d "Get the age of a file in seconds since it was last modified"
    if test ! -e "$file"
        return 1
    end
    
    if type -q perl
        perl -e "printf(\"%s\n\", time - (stat ('$file'))[9])" ^ /dev/null

    else if type -q python
        python -c "from __future__ import print_function; import os, time; print(int(time.time() - os.path.getmtime('$file')))" ^ /dev/null
    end
end
