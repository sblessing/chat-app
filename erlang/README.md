chatapp
=====

This implementation of the chat-app benchmark uses Erlang OTP state machines.
An implementation using only `spawn` and message-send operators would probably
be more performant; for the first version, we preferred the utility that the
OTP library gives us.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/chatapp -h
