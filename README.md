## practical-haskell

Notes, sample code and Excercises form the [practical-haskell book](https://www.apress.com/gp/book/9781484244791) by [Alejandro Serrano Mena](https://www.haskellers.com/user/serras)

## Usage

Excersize are completed using unit tests:

```sh
stack clean
stack build
stack test --file-watch
```

Some of the excecises in Chapter8, working with serveral cores, require [rabbitmq](https://hub.docker.com/_/rabbitmq).

```sh
cd ./scripts/rabbitmq
docker-compose up -d
```

## Future work

Convert this to a [nix](https://nixos.org/download.html#nix-quick-install) project
