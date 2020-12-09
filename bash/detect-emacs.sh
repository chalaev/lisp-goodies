
# Sometimes I want to call
# emacsclient -e '(great-function)'
# from my bash scripts.

# detectEmacs checks if emacs is available for the current user:
function detectEmacs(){
local emacs_detected=-1
for PIDfile in /tmp/emacs-*.pid; do
    if  [ -f $PIDfile ] && [ -O "$PIDfile" ]; then
	if [ $emacs_detected -ge 0 ]; then
	    # erasing extra PID file
	    rm -f $PIDfile
	else
	    local PID=`cat $PIDfile`
	    if `kill -s 0  $PID`; then
		emacs_detected=0
		echo $PID
	    else
		# erasing invalid PID file $PIDfile
		rm -f $PIDfile
	    fi
	fi
    fi
done
return $emacs_detected
}

# This bash script assumes that ~/.emacs
# creates /tmp/emacs-*.pid file, see generated/dot.emacs
