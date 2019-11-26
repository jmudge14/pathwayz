# pathwayz

Implements a basic web version of the Pathwayz board game.

In addition, it implements the start of a primitive neural network as one attempt to create an AI player. Read sigmoid.lisp if interested, but this is not particularly useful in its current state. (It will, however, "learn" to play, poorly - never gets much past simply making legal moves.)




## Usage

## Installation

1. Clone into ~/quicklisp/local-projects (not in quicklisp distribution yet)
2. (ql:quickload :pathwayz)
3. Start the service with defaults: (pathwayz:start)

This will run the service in a separate thread, which by default is at http://localhost:5000

## Author

* Jack Mudge <jakykong@theanythingbox.com>

## Copyright

Copyright (c) 2017 Jack Mudge (jakykong@theanythingbox.com)

## License

Public domain, to whatever extent legally allowed.

