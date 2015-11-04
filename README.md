### Description

This is a rewrite of my space invaders game that I wrote in Clojure; you can see that project [here](https://github.com/quephird/space-invaders).

### Getting it running

* Make sure you have PureScript installed; if you don't, go [here](http://www.purescript.org/download/) for directions.

* Make sure you have pulp installed; if you don't, go [here](https://github.com/bodil/pulp) for directions.

* Clone this repository:  

  ```
  git clone https://github.com/quephird/space-invaders-ps
  ```

* Change into the new directory and run this to bring down all of the dependencies:  

  ```
  pulp dep install
  ```

* Insure that the `dist` subdirectory exists; if it doesn't create it otherwise the next step will fail. (You will only need to do this once.)  

* Run the following to compile all of the source code for the game:  

  ```
  pulp build -O --main Main --to dist/Main.js
  ```

* Open the main HTML file in the browser by navigating to ``$PROJECT_DIRECTORY/html/Main.html`.

### Playing it live right now

Welp, go right [here](http://quephird.github.io/games/space-invaders/html/Main.html).
