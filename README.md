# calculate

 [ ![Download](https://api.bintray.com/packages/ajatix/scala/calculate/images/download.svg) ](https://bintray.com/ajatix/scala/calculate/_latestVersion)

A Scala implementation of a calculator to illustrate Functional Programming concepts like ADTs, Implicits and Typeclasses

## Using the library in a new project
Add the following to your `build.sbt`

```scala
resolvers += Resolver.bintrayRepo("ajatix", "scala")

libraryDependencies += "io.github.ajatix" %% "calculate" % "0.2"
```

In your main application, your can run the following

```scala
import io.github.ajatix.calculate.ExpressionDSL._

object Calculate extends App {

  val e = 3 into 4 plus 3 by 3 minus 4

  println(e)
  println(e.reorder())
  println(e.reorder().optimize())
  println(e.reorder().optimize().evaluate())

}
```
