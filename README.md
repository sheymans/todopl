# todopl

This is a task manager that schedules work for tasks automatically. It's a
desktop application using Clojure's [seesaw](https://github.com/daveray/seesaw)
on top of a [SWI-Prolog](http://www.swi-prolog.org/) server.

`todopl` actually started out as [Sheldonize](https://sheldonize.com), but was
taken out of production in favor of a Django-based web application (which is
still alive and kicking at [Sheldonize](https://sheldonize.com)).

Contributions to `todopl` are most welcome (see below); I think it serves as a
nice use case for combining SWI-Prolog with a Clojure desktop application.

## Requirements

This was tested on a

    Linux 3.13.0-39-generic #66-Ubuntu SMP Tue Oct 28 13:30:27 UTC 2014 x86_64 x86_64 x86_64 GNU/Linux

However, it should work on most systems given the following requirements (most
can be relaxed) are satisfied:

- Java 7 JDK
- [SWI-Prolog 6.6.4](http://www.swi-prolog.org/)
- [Leiningen 1.7.1](http://leiningen.org/)
- [Maven 3.0.5](http://maven.apache.org/)

## Installation

    ./build.sh

## Running

    ./run.sh

## How to Contribute

Fork this project, and create a branch with your changes. Next, do a pull
request. I'll check those regularly and merge your updates if appropriate.
Thanks a lot for playing around!

## License

GNU General Public License, Version 3 and higher.

