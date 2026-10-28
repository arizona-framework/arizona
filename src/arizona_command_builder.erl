-module(arizona_command_builder).
-moduledoc """
The behaviour of a command builder: a module whose functions build the
`t:arizona_effect:cmd/0` commands a client runs (`arizona_js` for the web,
`arizona_android` for `?json` views, `arizona_effect` for the socket protocol's own).

The parse transform recognises a builder by this behaviour, so a builder from any
package takes part without the transform naming it: a call to one in an event
attribute is proof the value is a command, which records the event name for the
client to delegate, and a call with literal arguments is folded into its encoded
bytes at compile time. Its functions therefore run in the compiler and must be
pure. A call to a builder whose module is not loadable yet (same app, clean build)
still records the name, so clean and incremental builds declare the same set; it
is just not folded.
""".

-doc """
The render target whose client runs this builder's commands, as its backend's
`c:arizona_renderer:target/0` names it, or `all` for commands every client runs
(the protocol's own). A builder's call in a template of another target is a
compile error: that template's client would receive commands it does not know.
""".
-callback target() -> atom() | all.
