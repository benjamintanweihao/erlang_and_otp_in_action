# TCP RPC Server

To run this code:

`iex -S mix`

Then, run the following in a terminal

```
telnet localhost 1055
Trying 127.0.0.1...
Connected to localhost
Escape character is '^]'.
:erlang.localtime()
{{2013,10,13},{23,46,45}}
:init.stop()
ok
Connection closed by foreign host.
```

Note that the current implementation doesn't accept any arguments. That means, doing something like `:lists.reverse([1,2,3])` will _not_ work.
