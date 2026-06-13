# MusicJukebox

MusicJukebox is a small cross-platform player for a local music folder (`mp3`, `wav` and `flac`).

The idea behind it is simple: music feels better when you do not hear the same tracks too often.
This player uses a forgetting-style approach, so familiar songs can disappear for a while and come back later feeling fresh again.

## What it does

The player keeps a small library file with the play history, the planned next play time, and the interval length for every track.

Right now, the interval logic is very simple: the interval grows by about 1.7x after each play.

There is also a small shuffle within the first third of the playlist, just to keep a bit of surprise.

## What you need

The program expects `ffplay.exe` to be available either in the working folder or in your system `PATH`.

FFmpeg download page:  
[https://ffmpeg.org/download.html](https://ffmpeg.org/download.html)

## How to use it

Put the executable into the folder with your music.

Put `ffplay.exe` into the same folder as well, or make sure it is available in your system `PATH`.

Then run the player from that folder.

The library file will be created automatically as `jukebox.json`.
You can add or remove tracks from your music folder, and the library will update automatically.

## Hotkeys

Playback is controlled with global hotkeys, so the console window does not need to be focused.

- `Ctrl + F7` — pause
- `Ctrl + F8` — resume
- `Ctrl + F9` — skip the current track

These hotkeys work on Windows and on Ubuntu with Wayland.

## Notes

No setup, no library import screen, and no extra UI.
Just drop the executable into a music folder and run it.

To display track names correctly in the Windows console, create a shortcut and run the program with code page 65001 (UTF-8), for example:

`cmd /K "chcp 65001 & R:\_JUKEBOX\musicjukebox-windows-x64.exe"`

TODO
- conseal debug message
- translate message
- print timeout after pause.
