# Пушка - Pushka

Пушка (Russian: Pushka, cannon) is a Push Notification proxy written in Haskell.

Puska will realy a push payload from an authorized sender to some receiver.
Authorized receivers register their device token with Pushka to receive pushes.

## Enviroment Dependencies

Pushka depends on [MongoDB](https://www.mongodb.org/) for storing device tokens and other information.

Pushka uses Amazon SNS to send pushes (WIP)

## Running Pushka

You need to have a MongoDB instance running somewhere and (for now) hardcode the adderess and the port in Pushka.

Pushka relies on [Stack](https://github.com/commercialhaskell/stack) for dependency management and build process.

To build and run Pushka do the following:

```
$ stack build && stack exec Pushka-exe
```
