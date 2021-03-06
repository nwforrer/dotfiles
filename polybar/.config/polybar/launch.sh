#!/bin/sh

# Terminate already running bar instances
killall polybar

# Wait until the processes have been shut down
# while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch the bars
polybar main &
polybar secondary &

echo "Bars launched..."
