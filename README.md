timetracker
===========

A small Erlang application to keep track of how long you have worked
the current day, without any user interaction. The purpose is to be
able to tell you when you have worked enough for a single day. A bit
like [hamster](https://github.com/projecthamster/).

It will not tell you _what_ you've worked on, how much _money_ you can
bill your customers, or any of that. The idea is (1) to tell you "hey,
you can stop working today", and (2) you shouldn't have to tell it
anything.

Currently it uses two ways to detect "activity":

1. `xinput` to detect mouse and keyboard activity
2. `lsmod | grep ^uvcvideo` to detect when a video device is active

After a "inactivity threshold", it will consider you to be "not
working", and stop the timer. As soon as it detects that you are
working again, it will resume the timer.

When you've worked more that your daily limit, it'll show a popup
telling you that it is time to stop working. This currently shown
using `xmessage`.

Platforms
---------

Although the Erlang parts should work with just OTP available, the
activity detection stuff assumes Linux at the bare minimum.

Configuration
-------------

There are some configuration options available in
`config/sys.config`. Perhaps the most interesting one is
`workday_length`.

Usage
-----

This is an Erlang program, so you'll need Erlang + rebar3 to run this.
To build the Erlang release, and start timetracker as a daemon, do:

    $ ./timetracker

If you want to see what it does, tail the log file:

    $ tail -f _build/default/rel/timetracker/timetracker.log
