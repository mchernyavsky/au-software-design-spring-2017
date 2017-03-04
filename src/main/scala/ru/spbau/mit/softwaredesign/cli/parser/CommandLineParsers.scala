package ru.spbau.mit.softwaredesign.cli.parser

import scala.util.parsing.combinator.RegexParsers

/** Regex-based parsers that convert a line to a composition of expressions */
object CommandLineParsers extends RegexParsers {
  private val compositionOperator = "|"
  private val assignmentOperator = "="
  private val substitutionOperator = "$"

  /** A parser that matches assignment or application */
  def expression: Parser[Expression] = assignment | application

  /** A parser that matches composition */
  def composition: Parser[Composition] = expression ~ opt(compositionOperator ~> composition) ^^ {
    case expr ~ compositionRest => Composition(expr, compositionRest.orNull)
  }

  /** A parser that matches assignment */
  def assignment: Parser[Assignment] = variable ~ (assignmentOperator ~> block) ^^ {
    case variable ~ block => Assignment(variable, block)
  }

  /** A parser that matches application */
  def application: Parser[Application] = rep1(block) ^^ {
    case function :: args => Application(function, args)
  }

  /** A parser that matches substitution or block */
  def block: Parser[Block] = substitution ^^ { Block(_) } | unQuotedLiteral ^^ { Block(_) } |
                             weakQuotedBlock | fullQuotedBlock

  /** A parser that matches substitution */
  def substitution: Parser[Substitution] = substitutionOperator ~> variable ^^
    (variable => Substitution(variable))

  /** Parsers that match literals */
  def variable: Parser[Literal] = """[A-Za-z_]\w*""".r ^^ { Literal }
  def unQuotedLiteral: Parser[Literal] = """[^'"$|\s]+""".r ^^ { Literal }
  def weakQuotedLiteral: Parser[Literal] = """[^"$]+""".r ^^ { Literal }
  def fullQuotedLiteral: Parser[Literal] = """[^']+""".r ^^ { Literal }

  /** Parsers that match quoted block */
  def weakQuotedBlock: Parser[Block] = "\"" ~> rep(substitution | weakQuotedLiteral) <~ "\"" ^^ { Block(_) }
  def fullQuotedBlock: Parser[Block] = "'" ~> rep(fullQuotedLiteral) <~ "'" ^^ { Block(_) }

  /** Parse all of String `line` with shell.parser `composition` */
  def parse(line: String): ParseResult[Composition] = parseAll(composition, line)
}
