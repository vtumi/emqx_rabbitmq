
emqx-web-hook
=============

EMQ X Webhook plugin.

##### emqx_rabbitmq.conf

```properties
rabbitmq.api.url = http://127.0.0.1:8080

## Encode message payload field
## rabbitmq.encode_payload = base64

rabbitmq.rule.client.connected.1     = {"action": "on_client_connected"}
rabbitmq.rule.client.disconnected.1  = {"action": "on_client_disconnected"}
rabbitmq.rule.client.subscribe.1     = {"action": "on_client_subscribe"}
rabbitmq.rule.client.unsubscribe.1   = {"action": "on_client_unsubscribe"}
rabbitmq.rule.session.created.1      = {"action": "on_session_created"}
rabbitmq.rule.session.subscribed.1   = {"action": "on_session_subscribed"}
rabbitmq.rule.session.unsubscribed.1 = {"action": "on_session_unsubscribed"}
rabbitmq.rule.session.terminated.1   = {"action": "on_session_terminated"}
rabbitmq.rule.message.publish.1      = {"action": "on_message_publish"}
rabbitmq.rule.message.deliver.1    = {"action": "on_message_deliver"}
rabbitmq.rule.message.acked.1        = {"action": "on_message_acked"}
```

API
----
* client.connected
```json
{
    "action":"client_connected",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "keepalive": 60,
    "ipaddress": "127.0.0.1",
    "proto_ver": 4,
    "connected_at": 1556176748,
    "conn_ack":0
}
```

* client.disconnected
```json
{
    "action":"client_disconnected",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "reason":"normal"
}
```

* client.subscribe
```json
{
    "action":"client_subscribe",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "topic":"world",
    "opts":{
        "qos":0
    }
}
```

* client.unsubscribe
```json
{
    "action":"client_unsubscribe",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "topic":"world"
}
```

* session.created
```json
{
    "action":"session_created",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117"
}
```

* session.subscribed
```json
{
    "action":"session_subscribed",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "topic":"world",
    "opts":{
        "qos":0
    }
}
```

* session.unsubscribed
```json
{
    "action":"session_unsubscribed",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "topic":"world"
}
```

* session.terminated
```json
{
    "action":"session_terminated",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "reason":"normal"
}
```

* message.publish
```json
{
    "action":"message_publish",
    "from_client_id":"C_1492410235117",
    "from_username":"C_1492410235117",
    "topic":"world",
    "qos":0,
    "retain":true,
    "payload":"Hello world!",
    "ts":1492412774
}
```

* message.deliver
```json
{
    "action":"message_delivered",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "from_client_id":"C_1492410235117",
    "from_username":"C_1492410235117",
    "topic":"world",
    "qos":0,
    "retain":true,
    "payload":"Hello world!",
    "ts":1492412826
}
```

* message.acked
```json
{
    "action":"message_acked",
    "client_id":"C_1492410235117",
    "username":"C_1492410235117",
    "from_client_id":"C_1492410235117",
    "from_username":"C_1492410235117",
    "topic":"world",
    "qos":1,
    "retain":true,
    "payload":"Hello world!",
    "ts":1492412914
}
```

License
-------

Apache License Version 2.0

Author
------

* [Sakib Sami](https://github.com/s4kibs4mi)

Contributors
------------

* [Deng](https://github.com/turtleDeng)
* [vishr](https://github.com/vishr)
* [emqplus](https://github.com/emqplus)
* [huangdan](https://github.com/huangdan)

