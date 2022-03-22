object Lambda {
  /* --- Structures --- */
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
      case DeBruijn.Abs(x, y) => s"λ ${y.toStr}"
      case DeBruijn.App(x, y) => s"(${x.toStr} ${y.toStr})"

    case FreeVar(name: String)
    case BoundVar(x: Int)
    case Abs(x: String, y: DeBruijn)
    case App(x: DeBruijn, y: DeBruijn)
  }

  /* --- Functions --- */
  def namedToDeBruijn(expr: Named): DeBruijn = expr match {
    case Named.Var(x) => DeBruijn.FreeVar(x)
    case Named.Abs(x, y) => aux(y, Map(x -> 0))
    case Named.App(x, y) => DeBruijn.App(aux(x, Map.empty), aux(x, Map.empty))
  }

  def aux(expr: Named, map: Map[String, Int]): DeBruijn = expr match {
    case Named.Var(x) => if map.contains(x) then DeBruijn.Abs(x, DeBruijn.BoundVar(map(x))) else DeBruijn.FreeVar(x)
    case Named.Abs(name, y) => DeBruijn.Abs(name, aux(y, map.map((key, value) => (key, value + 1)).updated(name, 0)))
    case Named.App(x, y) => DeBruijn.App(
      aux(x, map.map((key, value) => (key, value + 1))),
      aux(x, map.map((key, value) => (key, value + 1))))
  }

  def deBruijnToNamed(expr: DeBruijn): Named = expr match {
    case DeBruijn.FreeVar(x) => Named.Var(x)
    case DeBruijn.BoundVar(dist) => Named.Var(dist.toString)
    case DeBruijn.Abs(name, y) => Named.Abs(name, deBruijnToNamed(y))
    case DeBruijn.App(x, y) => Named.App(deBruijnToNamed(x), deBruijnToNamed(y))
  }

}
