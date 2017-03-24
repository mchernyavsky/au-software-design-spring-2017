package ru.spbau.mit.softwaredesign.cli.parser

import scala.util.parsing.combinator.RegexParsers

/** Regex-based parsers that convert a line to a composition of expressions */
object CommandLineParsers extends RegexParsers {
  private val compositionOperator = "|"
  private val assignmentOperator = "="
  private val substitutionOperator = "$"

  private def expression: Parser[Expression] = assignment | application

  private def composition: Parser[Composition] = expression ~ opt(compositionOperator ~> composition) ^^ {
    case expr ~ compositionRest => Composition(expr, compositionRest.orNull)
  }

  private def assignment: Parser[Assignment] = variable ~ (assignmentOperator ~> block) ^^ {
    case variable ~ block => Assignment(variable, block)
  }

  private def application: Parser[Application] = block ~ rep(block) ^^ {
    case function ~ args => Application(function, args)
  }

  private def block: Parser[Block] = substitution ^^ { Block(_) } | unQuotedLiteral ^^ { Block(_) } |
                                     weakQuotedBlock | fullQuotedBlock

  private def substitution: Parser[Substitution] = substitutionOperator ~> variable ^^
    (variable => Substitution(variable))

  private def variable: Parser[Literal] = """[A-Za-z_]\w*""".r ^^ { Literal }
  private def unQuotedLiteral: Parser[Literal] = """[^'"$|\s]+""".r ^^ { Literal }
  private def weakQuotedLiteral: Parser[Literal] = """[^"$]+""".r ^^ { Literal }
  private def fullQuotedLiteral: Parser[Literal] = """[^']+""".r ^^ { Literal }

  private def weakQuotedBlock: Parser[Block] = "\"" ~> rep(substitution | weakQuotedLiteral) <~ "\"" ^^ { Block(_) }
  private def fullQuotedBlock: Parser[Block] = "'" ~> rep(fullQuotedLiteral) <~ "'" ^^ { Block(_) }

  /** Parse all of String `line` with parser `composition` */
  def parse(line: String): ParseResult[Composition] = parseAll(composition, line)
}
