steamapps="/home/hunter/.steam/steam/SteamApps/"

list="steambrowser --list-names $steamapps"
output="$($list | dmenu -p 'Game' -fn -misc-fixed-medium-r-normal-*-22-200-*-*-*-*-*-*  -i -nf 'red' -nb 'black' -sf 'white' -sb 'red')"
appid=$(steambrowser --id "$output" $steamapps)
steam -applaunch $appid
