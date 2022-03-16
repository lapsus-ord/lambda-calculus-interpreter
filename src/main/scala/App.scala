object App {

  @main def main(): Unit =
    println("--- Lambda Interpreter ---")
    val t1 = Named.App(Named.Abs("x", Named.Var("x")), Named.Var("x"))
    println(s"t1 = ${t1.toStr}")
    println(s"t1_bruijn = ${namedToIndex(t1).toStr}")
    val t2 =
      Named.App(
        Named.Abs(
          "x", Named.Abs(
            "y", Named.App(
              Named.Abs(
                "z", Named.App(
                  Named.Abs("x", Named.Var("z")),
                  Named.Var("x"))),
              Named.App(
                Named.Abs("y", Named.Var("z")),
                Named.Var("y"))))),
        Named.App(Named.Var("x"), Named.Var("y")))
    println(s"t2 = ${t2.toStr}")
    val t3 = DeBruijn.App(
      DeBruijn.App(DeBruijn.Abs(DeBruijn.BoundVar(0)), DeBruijn.BoundVar(0)),
      DeBruijn.App(DeBruijn.Abs(DeBruijn.BoundVar(0)), DeBruijn.BoundVar(0)))
    println(s"t3 = ${t3.toStr}")

  enum Named {
    def toStr: String = this match
      case Named.Var(x) => x
      case Named.Abs(x1, x2) => s"λ$x1.${x2.toStr}"
      case Named.App(x1, x2) => s"(${x1.toStr} ${x2.toStr})"

    case Var(x: String)
    case Abs(x: String, y: Named)
    case App(x: Named, y: Named)
  }

  enum DeBruijn {
    def toStr: String = this match
      case DeBruijn.FreeVar(name) => ""
      case DeBruijn.BoundVar(x) => x.toString
      case DeBruijn.Abs(x) => s"λ ${x.toStr}"
      case DeBruijn.App(x, y) => s"(${x.toStr} ${y.toStr})"

    case FreeVar(name: String)
    case BoundVar(x: Int)
    case Abs(y: DeBruijn)
    case App(x: DeBruijn, y: DeBruijn)
  }

  def namedToIndex(expr: Named): DeBruijn = expr match {
    case Named.Var(x) => DeBruijn.FreeVar(x)
    case Named.Abs(x, y) => aux(y, Map(x -> 0))
    case Named.App(x, y) => DeBruijn.App(aux(x, Map.empty), aux(x, Map.empty))
  }

  def aux(expr: Named, map: Map[String, Int]): DeBruijn = expr match {
    case Named.Var(x) => if map.contains(x) then DeBruijn.Abs(DeBruijn.BoundVar(map(x))) else DeBruijn.FreeVar(x)
    case Named.Abs(x, y) => aux(y, map.map((key, value) => (key, value + 1)).updated(x, 0))
    case Named.App(x, y) => DeBruijn.App(aux(x, map.map((key, value) => (key, value + 1))),
      aux(x, map.map((key, value) => (key, value + 1))))
  }

}
