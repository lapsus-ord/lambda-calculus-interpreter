object App {

  enum VarNommees:
    case LambdaTerme(l: VarNommees)
    case Argument()
    case ValeurRetour()
    case VarExterne(arg: String)
    case Application(l: VarNommees.LambdaTerme, arg: VarNommees.VarExterne)

  def eval(expr: VarNommees) = expr match {
    case _ => ???
  }

  @main def main = println("hello")

}
