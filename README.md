# Slack Echo Bot.

Written for February 2016's [West London Hack Night](http://www.meetup.com/West-London-Hack-Night/).

## Building

You'll need [stack](https://github.com/commercialhaskell/stack).

Edit `app/Main.hs` to add your Slack bot API key, then call:

``` sh
stack build
stack exec slacky
```
