# Emacs Configuration

## Installation
Create repo in ```~/.emacs.d/```

## Run Emacs Daemon
To start an Emacs server (i.e. run Emacs as a daemon), execute ```systemctl --user enable emacs```.

To run an Emacs client, either run ```emacsclient -create-frame --alternate-editor=""```, or create a Desktop entry in ```.local/share/applications```:

```
[Desktop Entry]
Name = Emacs Client
GenericName=Text Editor
Comment=Emacs Editor
MimeType=text/english;text/plain
Exec=emacsclient -create-frame --alternate-editor=""
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
```
