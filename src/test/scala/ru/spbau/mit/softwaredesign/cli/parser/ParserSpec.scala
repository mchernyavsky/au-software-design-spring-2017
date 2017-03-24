package ru.spbau.mit.softwaredesign.cli.parser

import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {
  it should "parse assignment" in {
    val expected = Composition(
                      Assignment(
                        Literal("answer"),
                        Block(List(Literal("42")))),
                      null)
    val actual = CommandLineParsers.parse("answer=42").get
    assertResult(expected)(actual)
  }

  it should "parse application" in {
    val expected = Composition(
                      Application(
                        Block(List(Literal("cmd"))),
                        List()),
                      null)
    val actual = CommandLineParsers.parse("cmd").get
    assertResult(expected)(actual)
  }

  it should "parse application with arguments" in {
    val expected = Composition(
                      Application(
                        Block(List(Literal("cmd"))),
                        List(Block(List(Literal("arg1"))), Block(List(Literal("arg2"))))),
                      null)
    val actual = CommandLineParsers.parse("cmd arg1 arg2").get
    assertResult(expected)(actual)
  }

  it should "parse substitution" in {
    val expected = Composition(
                      Application(
                        Block(List(Substitution(Literal("var")))),
                        List()),
                      null)
    val actual = CommandLineParsers.parse("$var").get
    assertResult(expected)(actual)
  }

  it should "handle weak quotes" in {
    val expected = Composition(
                      Application(
                        Block(List(Literal("cmd"))),
                        List(Block(List(Substitution(Literal("arg1")), Substitution(Literal("arg2")))))),null)
    val actual = CommandLineParsers.parse("cmd \"$arg1 $arg2\"").get
    assertResult(expected)(actual)
  }

  it should "handle strong quotes" in {
    val expected = Composition(
                      Application(
                        Block(List(Literal("cmd"))),
                        List(Block(List(Literal("$arg1 $arg2"))))),
                      null)
    val actual = CommandLineParsers.parse("cmd '$arg1 $arg2'").get
    assertResult(expected)(actual)
  }

  it should "parse composition" in {
    val expected = Composition(
                      Application(
                        Block(List(Literal("cmd1"))),
                        List()),
                      Composition(
                        Application(
                          Block(List(Literal("cmd2"))),
                          List()),
                        null))
    val actual = CommandLineParsers.parse("cmd1 | cmd2").get
    assertResult(expected)(actual)
  }
}
