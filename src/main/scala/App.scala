import scala.annotation.tailrec
import Lambda._

object App {

  @main def main(): Unit =
    println("--- Lambda Interpreter ---")
    val kCombi = Named.Abs("x", Named.Abs("y", Named.Var("x")))
    println(s"kCombinator = ${kCombi.toStr}")
    println(s"kCombinator (bruijn) = ${namedToDeBruijn(kCombi).toStr}") // λ λ 1
    println(s"kCombinator (named) = ${deBruijnToNamed(namedToDeBruijn(kCombi)).toStr}")
    val sCombi = Named.App(
      Named.App(Named.Abs("x", Named.Abs("y", Named.Abs("z", Named.Var("x")))), Named.Var("z")),
      Named.App(Named.Var("y"), Named.Var("z")))
    println(s"sCombinator = ${sCombi.toStr}")
    println(s"sCombinator (bruijn) = ${namedToDeBruijn(sCombi).toStr}") // λ λ λ 2 0 (1 0)

}
