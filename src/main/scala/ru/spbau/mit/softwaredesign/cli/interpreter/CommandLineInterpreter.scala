package ru.spbau.mit.softwaredesign.cli.interpreter

import java.io._
import java.nio.channels._

import org.apache.commons.io.IOUtils
import resource._
import ru.spbau.mit.softwaredesign.cli.parser.{Unit => _, _}

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

/** Stores environment and performs command interpretation */
class CommandLineInterpreter {
  private val environment = mutable.Map[String, String]().withDefaultValue("")

  /** Evaluate a pipeline of commands */
  def eval(composition: Composition, in: InputStream = System.in, out: OutputStream = System.out): Unit = {
    composition match {
      case Composition(expr, null) => eval(expr, in, out)
      case Composition(expr, rest) =>
        val pipe = Pipe.open()
        for {
          sink <- managed(Channels.newOutputStream(pipe.sink))
          source <- managed(Channels.newInputStream(pipe.source))
        } {
          eval(expr, in, sink)
          sink.close()
          eval(rest, source, out)
        }
    }
  }

  private def eval(expr: Expression, in: InputStream, out: OutputStream): Unit = {
    expr match {
      case Assignment(Literal(value), block) =>
        environment(value) = processBlock(block)
      case Application(function, args) =>
        processBlock(function) match {
          case "cat" => evalCat(args, in, out)
          case "echo" => evalEcho(args, out)
          case "wc" => evalWc(args, in, out)
          case "pwd" => evalPwd(out)
          case "exit" => evalExit()
          case commandName => evalExternal(commandName, args, in, out)
        }
    }
  }

  private def evalCat(args: List[Block], in: InputStream, out: OutputStream): Unit = {
    if (args.isEmpty) {
      IOUtils.copy(in, out)
    } else {
      args.foreach { arg =>
        val fileName = processBlock(arg)
        for (fileIn <- managed(new FileInputStream(fileName))) {
          IOUtils.copy(fileIn, out)
        }
      }
    }
  }

  private def evalEcho(args: List[Block], out: OutputStream): Unit = {
    val concatArgs = args.map { processBlock } mkString " "
    out.write((concatArgs + '\n').getBytes)
  }

  private def evalWc(args: List[Block], in: InputStream, out: OutputStream): Unit = {
    def countLinesWordsBytes(text: String): (Int, Int, Int) = {
      val lines = text.split('\n').length
      val words = text.split("""\s+""").length
      val bytes = text.getBytes.length
      (lines, words, bytes)
    }

    args.foreach { arg =>
      val fileName = processBlock(arg)
      for (fileIn <- managed(new FileInputStream(fileName))) {
        val source = Source.fromInputStream(fileIn).mkString
        val (lines, words, bytes) = countLinesWordsBytes(source)
        out.write(s"$lines $words $bytes $fileName\n".getBytes)
      }
    }
  }

  private def evalPwd(out: OutputStream): Unit = {
    val userDir = System.getProperty("user.dir")
    out.write((userDir + '\n').getBytes)
  }

  private def evalExit(): Unit = {
    System.exit(0)
  }

  private def evalExternal(commandName: String, args: List[Block], in: InputStream, out: OutputStream): Unit = {
    val concatArgs = args.map { processBlock } mkString " "
    val logger = ProcessLogger(line => out.write((line + '\n').getBytes), _ => ())
    if (in.equals(System.in)) {
      s"$commandName $concatArgs" ! logger
    } else {
      s"$commandName $concatArgs" #< in ! logger
    }
  }

  private def processBlock(block: Block): String = block.units.map {
    case Literal(value) => value
    case Substitution(Literal(value)) => environment(value)
  }.mkString
}
