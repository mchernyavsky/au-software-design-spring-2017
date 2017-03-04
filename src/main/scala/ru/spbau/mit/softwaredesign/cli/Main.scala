package ru.spbau.mit.softwaredesign.cli

import java.io.IOException

import ru.spbau.mit.softwaredesign.cli.interpreter.CommandLineInterpreter
import ru.spbau.mit.softwaredesign.cli.parser.CommandLineParsers

object Main extends App {
  val cli = new CommandLineInterpreter
  Console.out.print("> ")
  for (line <- io.Source.stdin.getLines) {
    try {
      CommandLineParsers.parse(line) match {
        case CommandLineParsers.Success(pipeline, _) => cli.eval(pipeline)
        case CommandLineParsers.Failure(_, _) => ()
        case CommandLineParsers.Error(message, _) => Console.err.println(s"ParserError: $message")
      }
    } catch {
      case e: IOException => Console.err.println(s"IOException: ${e.getMessage}")
    }

    Console.out.print("> ")
  }
}
