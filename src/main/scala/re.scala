import scala.annotation.tailrec
import scala.util.parsing.combinator._

abstract class RegexExpr

case class Literal(c: Char)                       extends RegexExpr // a, b
case class Or(exprA: RegexExpr, exprB: RegexExpr) extends RegexExpr // a | b
case class Concat(a: RegexExpr, b: RegexExpr)     extends RegexExpr // abc -> Concat(a, Concat(b, c))
case class Repeat(expr: RegexExpr)                extends RegexExpr // a*
case class Plus(expr: RegexExpr)                  extends RegexExpr // a+

/* operator binding order (strongest -> weakest):
 (1. char literals, parentheses)
  2. +, *
  3. concatenation
  4. |
*/

object RegexParser extends RegexParsers {

    def exprCharLiteral:   Parser[RegexExpr] = ("""\w""".r | ".") ^^ {c => Literal(c.head)}
    def exprParenthetical: Parser[RegexExpr] = "(" ~> prioLow <~ ")"
    def exprRepeat:        Parser[RegexExpr] = prioLow <~ "*" ^^ (l => Repeat(l))
    def exprPlus:          Parser[RegexExpr] = prioLow <~ "+" ^^ (l => Plus(l))
    def exprConcat:        Parser[RegexExpr] = rep(prioMid) ^^ {list => listToConcat(list)}
    def exprOr:            Parser[RegexExpr] = prioHigh ~ "|" ~ prioHigh ^^ {case l ~ "|" ~ r => Or(l, r)}

    def prioLow:  Parser[RegexExpr] = exprCharLiteral | exprParenthetical
    def prioMid:  Parser[RegexExpr] = exprRepeat | exprPlus | prioLow
    def prioHigh: Parser[RegexExpr] = exprConcat | prioMid
    def prioMax:  Parser[RegexExpr] = exprOr | prioHigh

    def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
        case head :: Nil       => head
        case head :: remainder => Concat(head, listToConcat(remainder))
    }

    def apply(in: String): Option[RegexExpr] = {
        parseAll(prioMax, in) match {
            case Success(out, _)    => Some(out)
            case failure: NoSuccess => None
        }
    }
}

abstract class State

class      Consume(val c: Char, val out: State)      extends State // reference equality
class      Split(val out_l: State, val out_r: State) extends State // reference equality
class      Placeholder(var pointTo: State)           extends State // 
case class Match()                                   extends State // value-based equality
