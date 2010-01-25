Scalabos
========

**Scalabos** is an experimental Scala implementation of [Lattice-Boltzmann method](http://www.lbmethod.org/) (LBM), an alternative to differential equation to address fluid dynamics problems.

Its name is a stupid pun on **Palabos**, a [free and efficient](http://www.lbmethod.org/palabos) LBM implementation.

Installation
------------

The current release is an experimental-pre-alpha-proof-of-concept
release. So its prehaps better to wait a bit... Honestly, it is not
able to compute the physics correctly but it is already can produce
nice (but useless) pictures.

If you want to give it a try, you will need to:

1. Install the build tool: [sbt](http://code.google.com/p/simple-build-tool/)
2. Clone the git repository
3. Run: `sbt update` to download automatically the dependencies
4. Run: `sbt compile` to compile the project

Good luck with that...

Dependencies
------------

Scalabos uses [JFreeChart](http://www.jfree.org/jfreechart/) for picture drawing.

License
-------

It will be soon released with a FOSS license. For the moment, if you want to import code in other projects, redistribute it, etc., you will need to wait...