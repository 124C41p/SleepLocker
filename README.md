# SleepLocker

Priority softlock system designed for (but not limited to) WoW Classic. Currently only in German.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

SleepLocker depends on the runtime engine `Node.js` which can be obtained [here](https://nodejs.org).
If you plan building the project on an ARM architecture (e.g. on a Raspberry Pi), you also have to install the `Elm` compiler manually (see below).

### Installing

Get a local copy of SleepLocker an run the following commands inside the project directory:

```
npm install
npm run build:prod
```

### Installing on ARM architecture

The SleepLocker frontend is written in `Elm`. Unfortunately, to the present day there are no official ARM-binaries of the `Elm` compiler available.
In order to build the project you either have to compile `Elm` yourself (not recommended) or to use [unofficial binaries](https://github.com/dmy/elm-raspberry-pi).


### Running

After installing, the SleepLocker webserver can be started by running

```
npm start
```

in the project directory. The server will listen on port 12345.

## Author

Clester / Mogrimm @ Dragon's Call

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
