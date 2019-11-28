# erl_ofc

erl_ofc is OpenFlow 1.3 library implemented by Erlang.

## Feature

- Support OpenFlow 1.3
- Listen OpenFlow Channel
- Decode and encode OpenFlow 1.3 binary message to/from erlang term
- Send OpenFlow messages to application via `gen_server` message cast
- JSON encode and decode for OpenFlow Messages

## Use

Most easy and recommended way, append this repository to your OpenFlow Application's build system.
ex) Rebar3, Erlang.mk

Otherwise, example for development, this library has build using Erlang.mk.
Simply run `make` and append library directory to Erlang runtime library directory path.

## Example

TBD
