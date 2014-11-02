erlgcm
======

Erlang library to interact with Google Cloud Messaging.

What is it?
===========

Google Cloud Messaging is used as a Push Notification system on Android platforms.
This library allows you to send push notification messages to Android phones, provided
you have an API key (http://developer.android.com/google/gcm/gs.html#access-key) and
the registration IDs sent by the app on the phones.

Currently the only protocol supported is JSON over HTTP, which provides only server -> client
communication. In the future I hope to support CCS using XMPP (send a pull request!)
which allows server <-> client communication.

Example
=======

```
1> application:set_env(erlgcm, api_key, "foo").
2> Data = [{text, "hello"}].
3> M = gcm_message:new([{data, Data}]).
4> gcm_sender:send(M, ["registration_id"]).
```