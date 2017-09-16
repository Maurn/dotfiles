#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

hostname=$(hostname)
if [ $hostname == "spanden" ]; then
    # Launch bar1 and bar2
    MONITOR=DVI-D-0 polybar -r bottom &
    MONITOR=DVI-I-1 polybar -r bottom &
else
    MONITOR=LVDS-1 polybar -r bottom &
fi

echo "Bars launched..."
