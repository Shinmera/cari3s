## About Cari3s
This is an [i3](https://i3wm.org) status bar generator library/program.

## How To
First make sure Cari3s is available to ASDF by either cloning and registering it, or downloading it via Quicklisp. Then build a standalone binary of Cari3s like this:

    sbcl --dynamic-space-size 128MB --eval "(asdf:make :cari3s)"

This should produce a binary called `cari3s` in the source directory. Copy or symlink it into your `PATH`.

Next, create a configuration file in `~/.config/i3/cari3s.conf` with something like the following:

    (cpu-usage)
    (mem-usage)
    (io-usage)
    (network-ip)
    (clock)

Then change your `~/.config/i3/config`'s status bar section and set the `status_command` to `cari3s`:

    bar {
      status_command cari3s
      ...
    }

Once you reload i3, it should display a status bar similar to the following:

![status-bar](https://filebox.tymoon.eu//file/TVRVNU5nPT0=)

In the above example we defined five generators. The following generators are available out of the box:

* `battery`
* `clock`
* `cpu-usage`
* `disk-usage`
* `io-usage`
* `mem-usage`
* `network-ip`
* `uptime`

The look of each of them can be customised through the `:text` format string, and the `:markup` [pango options](https://shinmera.github.io/pango-markup), as well as a variety of extra options for `block`s.

## Defining new Generators
Defining a raw generator is just a matter of subclassing `generator` and providing a method for `generate` that, when invoked, returns a list of `block` instances that should be displayed on the status bar.

There's some helper classes available like the `single-generator` and the `value-generator`, as most generators will only produce a single block and produce their text based on some recomputed value every time. 

For the `value-generator` all you need to implement is a method on `compute-value` and provide a sensible default `:text` initarg.

None of the existing generators are terribly complicated, so if you're still unsure what to do, have a look at their sources.
