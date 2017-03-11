package ru.spbau.mit.softwaredesign.cli.parser

/** <expression> ::= <assignment> | <application> */
sealed trait Expression

/** <composition> ::= <expression>, [ <composition> ] */
case class Composition(expression: Expression, compositionRest: Composition) extends Expression

/** <assignment> ::= <variable>, "=", <block> */
case class Assignment(variable: Literal, value: Block) extends Expression

/** <application> ::= <block>, { <block> } */
case class Application(function: Block, args: List[Block]) extends Expression

/** <block> ::= <unit> | """, { <unit> }, """ | "'", <literal>, "'" */
case class Block(units: List[Unit]) extends Expression {
  def this(unit: Unit) = this(List(unit))
}

/** Overloaded `apply` method for constructing Block from Unit */
object Block {
  def apply(unit: Unit): Block = new Block(unit)
}

/** <unit> ::= <literal> | <substitution> */
sealed trait Unit extends Expression

/** <literal> ::= { <character> } */
case class Literal(value: String) extends Unit

/** <substitution> ::= "$", <variable> */
case class Substitution(variable: Literal) extends Unit
