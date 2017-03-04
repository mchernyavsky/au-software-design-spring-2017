package ru.spbau.mit.softwaredesign.cli.parser

sealed trait Expression

case class Composition(expression: Expression, compositionRest: Composition) extends Expression

case class Assignment(variable: Literal, value: Block) extends Expression
case class Application(function: Block, args: List[Block]) extends Expression

case class Block(units: List[Unit]) extends Expression {
  def this(unit: Unit) = this(List(unit))
}

object Block {
  def apply(unit: Unit): Block = new Block(unit)
}

sealed trait Unit extends Expression

case class Literal(value: String) extends Unit
case class Substitution(variable: Literal) extends Unit
