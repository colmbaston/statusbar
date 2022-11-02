# statusbar

A text-based status bar written in Haskell.
It was written solely for my personal use with the [i3](https://i3wm.org/) and [Sway](https://swaywm.org/) window managers on my systems running [Arch Linux](https://www.archlinux.org/), so customisability is very limited outside of editing the source code, and it may depend on external packages that are unavailable on other systems.

## Usage

The program should be called with command-line arguments constituting a space-separated list of blocks. The output for the listed blocks should then be displayed in the order listed, with a new line printed each time the output text for one of the blocks updates.

```
> ./statusbar volume battery datetime
| Volume: Muted | Battery: 86% | Sunday 20/05/2018 @ 00:16
| Volume: Muted | Battery: 86% | Sunday 20/05/2018 @ 00:17
| Volume: Muted | Battery: 85% | Sunday 20/05/2018 @ 00:17
| Volume: Muted | Battery: 85% | Sunday 20/05/2018 @ 00:18
...
```

## Blocks

Blocks are named `IO` actions which each independently execute in their own thread.
Periodically, a block will write some text to an `MVar` shared by all threads, prompting the main thread to update the text of the statusbar if anything has changed.
All blocks are currently implemented in the same way:

* A command to obtain the desired information from the external system. These are run by the [`System.Process`](https://hackage.haskell.org/package/process) library.
* A parser to transform the raw output of the command into the string which will actually be displayed. This uses the [`Data.Attoparsec.Text`](https://hackage.haskell.org/package/attoparsec) library.
* Logic to determine when to re-run the command and parser, pushing the output to the main thread.

The currently implemented blocks and the external commands they depend upon are listed below.

| Block Name   | Command                                                                         |
|--------------|---------------------------------------------------------------------------------|
| `battery`    | [`acpi`](https://www.archlinux.org/packages/community/x86_64/acpi/)             |
| `datetime`   | `date`                                                                          |
| `dropbox`    | [`dropbox-cli`](https://aur.archlinux.org/packages/dropbox-cli/)                |
| `solaredge`  | [`curl`](https://archlinux.org/packages/core/x86_64/curl/)                      |
| `sync`       | [`inotifywait`](https://archlinux.org/packages/community/x86_64/inotify-tools/) |
| `playerctl`  | [`playerctl`](https://www.archlinux.org/packages/community/x86_64/playerctl/)   |
| `volume`     | [`amixer`](https://www.archlinux.org/packages/extra/x86_64/alsa-utils/)         |

Additionally, the `solaredge` block requires the `SOLAR_EDGE_SITE` and `SOLAR_EDGE_API_KEY` environment variables to be set to gain access to the [SolarEdge](https://monitoring.solaredge.com) monitoring API.
