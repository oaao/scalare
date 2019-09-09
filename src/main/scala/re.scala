import scala.collection.mutable
import scala.util.parsing.combinator._


abstract class RegexExpr

case class Literal(c: Char)                       extends RegexExpr // e.g. a, b, .
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
class      Split(val outL: State, val outR: State)   extends State // reference equality
class      Placeholder(var pointTo: State)           extends State // binding to pass context through a Repeat; allows cyclicality
case class Match()                                   extends State // case class for value-based equality


object NFA {
    // non-deterministic finite automata state machine; real fun to say, too

    def regexToNFA(re: RegexExpr): State = regexToNFA(re, Match())

    private def regexToNFA(re: RegexExpr, andThen: State): State = {

        re match {
            case Literal(c)   => new Consume(c, andThen)
            case Or(l, r)     => new Split(regexToNFA(l, andThen), regexToNFA(r, andThen))
            case Concat(a, b) => {regexToNFA(a, regexToNFA(b, andThen))} // convert first item to NFA, output result of converting second to NFA
            case Repeat(r)    =>
                val placeholder = new Placeholder(pointTo=null)
                val split       = new Split(regexToNFA(r, placeholder), andThen)
                // one path to placeholder, the other back back to r
                placeholder.pointTo = split
                placeholder
            case Plus(r)      => regexToNFA(Concat(r, Repeat(r)), andThen)
        }

    }
}

object NFAEvaluator {

    def evaluate(nfa: State, input: String): Boolean =
        evaluate(Set(nfa), input)

    def evaluate(multipleNfas: Set[State], input:String): Boolean = {
        input match {
            case "" =>
                evaluateStates(multipleNfas, None).exists(_ == Match())
            case string =>
                evaluate(
                    evaluateStates(multipleNfas, input.headOption),
                    string.tail
                )
        }
    }

    def evaluateStates(multipleNfas: Set[State], input: Option[Char]): Set[State] = {

        val visitedStates = mutable.Set[State]()

        multipleNfas.flatMap { state =>
            evaluateState(state, input, visitedStates)
        }
    }

    def evaluateState(currentState: State, input: Option[Char],
                      visitedStates: mutable.Set[State]): Set[State] = {

        if (visitedStates contains currentState) {
            Set()
        } else {
            visitedStates.add(currentState)

            currentState match {
                case placeholder: Placeholder =>
                    evaluateState(placeholder.pointTo, input, visitedStates)
                case consume: Consume =>
                    if (Some(consume.c) == input || consume.c == '.') {
                        Set(consume.out)
                    } else {
                        Set()
                    }
                case s: Split =>
                    evaluateState(s.outL, input, visitedStates) ++
                    evaluateState(s.outR, input, visitedStates)
                case m: Match =>
                    if (input.isDefined) Set() else Set(Match())
            }
        }

    }

}

object Regex {
    def fullMatch(input: String, pattern: String): Boolean = {
        val parsed = RegexParser(pattern).getOrElse(
            throw new RuntimeException("Failed to parse regular expression")
        )
        val nfa = NFA.regexToNFA(parsed)
        NFAEvaluator.evaluate(nfa, input)
    }

    def matchAnywhere(input: string, pattern: String): Boolean =
        fullMatch(input, ".*" + pattern + ".*")
}
