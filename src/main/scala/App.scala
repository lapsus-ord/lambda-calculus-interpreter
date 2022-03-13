object App {

  enum Named:
    case Var(x: String)
    case Abstraction(x: Named, y: Named)
    case Application(x: Named, y: Named)

  def toString(expr: Named): String = expr match {
    case Named.Var(x) => x
    case Named.Abstraction(x1, x2) => s"λ${toString(x1)}.${toString(x2)}"
    case Named.Application(x1, x2) => s"(${toString(x1)} ${toString(x2)})"
    case _ => throw new IllegalArgumentException
  }

  @main def main(): Unit =
    println("--- Lambda Interpreter ---")
    val t1 = Named.Application(Named.Abstraction(Named.Var("x"), Named.Var("y")), Named.Var("y"))
    println(s"t1 = ${toString(t1)}")
    val t2 =
      Named.Application(
        Named.Abstraction(
          Named.Var("x"),
          Named.Abstraction(
            Named.Var("y"),
            Named.Application(
              Named.Abstraction(
                Named.Var("z"),
                Named.Application(
                  Named.Abstraction(Named.Var("x"), Named.Var("z")),
                  Named.Var("x"))),
              Named.Application(
                Named.Abstraction(Named.Var("y"), Named.Var("z")),
                Named.Var("y"))))),
        Named.Application(Named.Var("x"), Named.Var("y")))
    println(s"t2 = ${toString(t2)}")

}
