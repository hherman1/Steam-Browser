Steam-Browser
=============

CLI to browse installed steam games in the provided directory. Intended for use in scripting, although use as you wish. The example script "steamgame.sh" shows one use of the program with dmenu to create a quick steam game launcher. 

If it is desired and people want it, support for other app properties can be added (such as file size or other intricacies.) Although until then, this is what it is!

`USAGE: steambrowser [OPTIONS...] [DIRECTORIES...]`

eg.

```bash
steambrowser --list-names ~/.steam/steam/SteamApps #Will list installed game names seperated by newlines
steambrowser -n 250900 ~/.steam/steam/SteamApps # Will tell you the name of the game with ID 250900
steambrowser -i "The Binding of Isaac: Rebirth" ~/.steam/steam/SteamApps # Will tell you the id of the game
```

There is also the `--list-ids` flag as well as `--help`, and I'm willing to add more if theres any specifically people would like to have.

Enjoy
