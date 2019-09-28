## Connect Four
A [connect four](https://en.wikipedia.org/wiki/Connect_Four) game written in Haskell utilizing the [gloss](https://hackage.haskell.org/package/gloss) library for the user interface.

### Dependencies
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [opengl](https://www.opengl.org/)
* [glut](https://www.opengl.org/resources/libraries/glut/)

### An example of how to use on Arch Linux
```
$ pacman -S --needed stack git mesa freeglut
$ git clone https://github.com/Disco-Dave/connect-four.git
$ cd connect-four
$ stack run
```

### Where are the unit tests?
I have not got around to writing any unit tests for this project. However I do plan on writing unit tests in the future using [quickcheck](https://hackage.haskell.org/package/QuickCheck) and [hspec](https://hackage.haskell.org/package/hspec).
