This project is a simulator and AI for the sublime strategy game Tak created by Fames Ernest and Patrick Rothfuss based on the fantasy novel 'The Wise Man's Fear'.

The project is written in Scala and built using SBT.

# Tak
Tak's rules are fairly simple and easy to catch up on. The complexity in terms of the branching factory is comparable to chess and immensely lower than Go. 

Therefore, it is a great opportunity to develop an artificial opponent. As opposed to chess, there are not myriads of strategies, simulators, AIs and whatnots online. So there is still room for experiments for young developers.

This project is primarily designed as an interface for developers to get an easily accessible entrance-point for their AI. 

Secondarily, one can play against the AI or friends, but real games are far more interesting nonetheless.

# Usage
The project is built using `sbt`. Make sure you have a version installed, that has been released after the bronze age at the earliest. Older versions might screw up.

After the installation run `sbt` in the project's root directory to start the interactive interface. 

Type `test` to run the tests, `clean` to clean up, and `run` to start the simulator. 

In addition to `run` you need to pass one or more parameters based on the interaction you desire. The options are described below.

There are two major ways to start the simulator.
1. Using an interface
    Currently, there is only one interface; a command line interface for playing the game in a rudimentary fashion. Pass `-cli` and get going.
2. Using a mode
    Modes are used to run any sort of tests. Those might be for debugging a specific AI or let several play against each over to acquire statistical data. 
    Pass `-debug -mode ${MODE_SPECIFIER}` to start the respective mode.
    As the project is still under development, refer to the code to see which modes are already available.

# Structure
The code consists of four major components. The most interesting components of those are explained in the following.

### Simulator
The [Simulator](src/main/scala/simulator/) package contains all the game's logic and interfaces for developers.

- The [Player](src/main/scala/simulator/Player.scala) trait must be extended to connect an AI to the Simulator.
- The [Analysis](src/main/scala/simulator/analysis/) package contains interfaces for analytical data acquisition. Those are only convenience methods.
- The [Game State](src/main/scala/simulator/GameState.scala) contains all necessary information about the current state of a game. The copy given to the player should not be changed, however, it provides a copy method so that changes can be applied.
- The [Simulator](src/main/scala/simulator/Simulator.scala) starts and manages the Simulation.

### AI
The [AI](src/main/scala/ai/) package contains everything related to AI, i.e. several search strategies, game evaluators and classifiers, and similar.

- The [Evaluation](src/main/scala/ai/evaluation) package contains several evaluators that compute a score for a given player in a given game state. This score estimated the quality of the state.
- The [Search](src/main/scala/ai/search.scala) package contains several generic search strategies which can be equipped with different evaluation methods and supplier for applicable actions so that some actions can excluded from the search. This exclusion can be based on the given game state.

### Interfaces
The [Interfaces](src/main/scala/interfaces/) package specifies everything related to an interface for a user.

This is currently rather sparsely populated and can be extended with new modes.

### Parsing
The [Parsing](src/main/scala/parsing/) package contains all kinds of parsers. They are placed in this directory because they are rather verbose and can potentially be used for several modules.

- The [Action Parser](src/main/scala/parsing/action/) allows to parse a string input into an action.
- The [State Parser](src/main/scala/parsing/state/) allows to parse a string representation of a state. This is a nifty way to quickly define a game state for tests. It makes test easier to read, understand, and therefore maintain.

# Author
Maximilian Schwenger ([email](mailto:schwenger@stud.uni-saarland.de), [github](https://github.com/Schwenger))

# Tak in real live
I do not own any rights for this game, nor did I contribute in its development.
Please refer to [kickstarter](https://www.kickstarter.com/projects/cheapassgames/tak-a-beautiful-game) to support the game and check out the King Killer Chronicles and Rothfuss in general if you have not already ([link](http://www.patrickrothfuss.com/content/books.asp)). 
