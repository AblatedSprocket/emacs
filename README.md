# Emacs Configuration

## Installation
Create repo in ```~/.emacs.d/```
On first install, uncomment the line ```(package-initialize)``` in the ```init.el``` file. This line can be commented out again once Emacs successfully runs.

For Rust development, download latest rust-analyzer release and place in ```~/.cargo/bin```. Add executable permissions.

For Python development, install python language server: ```pip3 install 'python-language-server[all]'```

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
