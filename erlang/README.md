chatapp
=====

This implementation of the chat-app benchmark uses Erlang OTP state machines.
An implementation using only `spawn` and message-send operators would probably
be more performant, but we aimed for clarity of the state machines.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/chatapp
