## Introduction

Comets! A simple Asteroids clone

## Playing

First! Get the game! You can grab it from the [Releases](https://github.com/thenandagain/comets/releases/) or by cloning and building it yourself:

```sh
git clone https://github.com/thenandagain/comets
cd comets
lein trampoline run
```
## Controls

  - `left-arrow` - Rotate Left
  - `right-arrow` - Rotate Right
  - `up-arrow` - Accelerate in the direction you're facing
  - `space` - fire your cannon
  - `r` - restart the game


## Contents

* `desktop/resources` Images, audio, and other files
* `desktop/src` Desktop-specific code
* `desktop/src-common` Cross-platform game code

## Building

All projects can be built using [Nightcode](https://nightcode.info/), or on the command line using [Leiningen](https://github.com/technomancy/leiningen) with the [lein-droid](https://github.com/clojure-android/lein-droid) and [lein-fruit](https://github.com/oakes/lein-fruit) plugins.
