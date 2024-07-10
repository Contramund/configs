if [[ $(cat /sys/class/power_supply/BAT1/status) = "Discharging" ]]
then
    notify-send -u critical 'Battery running out!!'
    mpv /home/contramund/.config/xmobar/battery-out-sound.mp3
fi
