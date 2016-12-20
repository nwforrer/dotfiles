#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/div/null; do sleep 1; done

# Launch the bars
polybar main &
polybar second &

echo "Bars launched..."
