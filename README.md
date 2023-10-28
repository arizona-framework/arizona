# Arizona

Arizona is a Web Framework for Erlang.

## Table of contents

- [Project goal](#project-goal)
- [Motivation](#motivation)
- [Roadmap](#roadmap)
- [Dependencies](#dependencies)
    - [Maintained by me](#maintained-by-me)
    - [Third-party](#third-party)
- [License](#license)

## Project goal

The goal of this project is to have a web framework based on the [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html), where users connect to the server and send and receive events in realtime via WebSocket.

## Motivation

Phoenix has an important role in the Elixir's popularity and community growth, and when the community grows, the language also grows. Erlang has a wonderful community and a strong and rock-solid structure, but I wanna see Erlang also growing fast and becoming more and more popular to make this world a better and peaceful place. This is the reason and motivation for this project.

## Roadmap

- [ ] Server side
    - [ ] Server
    - [ ] Request handler
    - [ ] WebSocket
    - [ ] JSON parser
    - [ ] Template compiler
    - [ ] Router
- [ ] Client side
    - [ ] Broker
    - [ ] WebWorker
    - [ ] Event bus
    - [ ] Realtime updates

## Dependencies

### Maintained by me

- [eel](https://github.com/williamthome/eel): Template compiler.

### Third-party

- [cowboy](https://github.com/ninenines/cowboy): Server;
- [sync](https://github.com/rustyio/sync.git): Hot code reload.

## License

Arizona is 100% open source and community-driven. All components are available under the Apache 2 License on [GitHub](https://github.com/spawnfest/arizona).

See [LICENSE.md](LICENSE.md) for more information.
