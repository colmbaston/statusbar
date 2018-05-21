# statusbar

A text-based status bar written in Haskell. It was written solely for my own use with the [i3 window manager](https://i3wm.org/) on [Arch Linux](https://www.archlinux.org/), so customisability is very limited outside of editing the source code, and it may depend on external packages that are unavailable on other systems.

## Usage

The program should be called with command-line arguments constituting a space-separated list of blocks. The output for the listed blocks should then be displayed in the order listed.

```
> ./statusbar volume battery datetime
| Volume: Muted | Battery: 86% | Sunday 20/05/2018 @ 00:16
| Volume: Muted | Battery: 86% | Sunday 20/05/2018 @ 00:17
| Volume: Muted | Battery: 85% | Sunday 20/05/2018 @ 00:17
| Volume: Muted | Battery: 85% | Sunday 20/05/2018 @ 00:18
...
```

## Blocks

Blocks are internally represented as the product of three components:

* A command to obtain the desired information from the external system. These are run by the [`System.Process`](https://hackage.haskell.org/package/process) library.
* A parser to transform the raw output of the command into the string which will actually be displayed. This uses the [`Data.Attoparsec.Text`](https://hackage.haskell.org/package/attoparsec) library.
* A thread which is forked to periodically request the update loop to re-run the command and parser. Threads achieve this by putting the array index of their block into an `MVar Int` shared by all threads.

The currently implemented blocks and the external commands they depend upon are listed below.

| Block Name | Command                                                                 |
|------------|-------------------------------------------------------------------------|
| battery    | [`acpi`](https://www.archlinux.org/packages/community/x86_64/acpi/)     |
| datetime   | `date`                                                                  |
| dropbox    | [`dropbox-cli`](https://aur.archlinux.org/packages/dropbox-cli/)        |
| volume     | [`amixer`](https://www.archlinux.org/packages/extra/x86_64/alsa-utils/) |
