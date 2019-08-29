abstract class RegexExpr

case class Literal(c: Char)                       extends RegexExpr // a, b
case class Or(exprA: RegexExpr, exprB: RegexExpr) extends RegexExpr // a | b
case class Concat(a: RegexExpr, b: RegexExpr)     extends RegexExpr // abc -> Concat(a, Concat(b, c))
case class Repeat(expr: RegexExpr)                extends RegexExpr // a*
case class Plus(expr: RegexExpr)                  extends RegexExpr // a+

