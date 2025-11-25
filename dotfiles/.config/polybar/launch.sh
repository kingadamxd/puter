#!/bin/sh

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

echo "*~NEW-POLYBAR-INSTANCE~*" | tee -a /tmp/polybar.log 

exec polybar puter 2>&1 | tee -a /tmp/polybar.log
