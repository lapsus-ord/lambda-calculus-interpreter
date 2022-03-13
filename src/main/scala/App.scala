object App {

  @main def main(): Unit =
    println("--- Lambda Interpreter ---")
    val t1 = Named.Application(Named.Abstraction(Named.Var("x"), Named.Var("y")), Named.Var("y"))
    println(s"t1 = ${t1.toStr}")
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
    println(s"t2 = ${t2.toStr}")

  enum Named {
    def toStr: String = this match
      case Named.Var(x) => x
      case Named.Abstraction(x1, x2) => s"λ${x1.toStr}.${x2.toStr}"
      case Named.Application(x1, x2) => s"(${x1.toStr} ${x2.toStr})"

    case Var(x: String)
    case Abstraction(x: Named, y: Named)
    case Application(x: Named, y: Named)
  }

}
