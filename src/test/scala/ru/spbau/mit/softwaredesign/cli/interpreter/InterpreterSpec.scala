package ru.spbau.mit.softwaredesign.cli.interpreter

import java.io.{InputStream, OutputStream}
import java.net.URLDecoder
import java.nio.channels.{Channels, Pipe}

import org.scalatest.FlatSpec
import resource.managed
import ru.spbau.mit.softwaredesign.cli.parser.{CommandLineParsers, Composition}

import scala.io.Source
import scala.util.Properties

class InterpreterSpec extends FlatSpec {
  def withInterpreter(test: CommandLineInterpreter => Any): Any = {
    val cli = new CommandLineInterpreter
    test(cli)
  }

  def withPipe(test: (OutputStream, InputStream) => Any): Any = {
    val pipe = Pipe.open()
    for {
      sink <- managed(Channels.newOutputStream(pipe.sink))
      source <- managed(Channels.newInputStream(pipe.source))
    } {
      test(sink, source)
    }
  }

  def parseCommandLine(commandLine: String): Composition = {
    CommandLineParsers.parse(commandLine) match {
      case CommandLineParsers.Success(pipeline, _) => pipeline
      case _ => fail
    }
  }

  def evalCommandLine(commandLine: String, cli: CommandLineInterpreter, sink: OutputStream): Unit = {
    val parsedCommandLine = parseCommandLine(commandLine)
    cli.eval(parsedCommandLine, System.in, sink)
    sink.close()
  }

  def evalCdCommand(commandLine: String, cli: CommandLineInterpreter, sink: OutputStream) : Unit ={
    val userDir = System.getProperty("user.dir")
    cli.eval(parseCommandLine("cd " + userDir), System.in, sink)
    val parsedCommandLine = parseCommandLine(commandLine)
    cli.eval(parsedCommandLine, System.in, sink)
    cli.eval(parseCommandLine("pwd"), System.in, sink)
    sink.close()
  }

  def evalLsCommand(commandLine : String, cli: CommandLineInterpreter, sink:OutputStream) : Unit ={
    val testDirPath = URLDecoder.decode(getClass.getResource("/test_folder").getPath, "UTF-8")
    cli.eval(parseCommandLine("cd " + testDirPath), System.in, sink)
    cli.eval(parseCommandLine(commandLine), System.in, sink)
    sink.close()
  }

  it should "interpret echo with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a b c"
      evalCommandLine(commandLine, cli, sink)
      val expected = "a b c" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret echo with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo"
      evalCommandLine(commandLine, cli, sink)
      val expected = Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret cat with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testFilePath = getClass.getResource("/lorem_ipsum.txt").getPath
      val commandLine = s"cat $testFilePath"
      evalCommandLine(commandLine, cli, sink)
      val expected = Source.fromFile(URLDecoder.decode(testFilePath, "UTF-8")).mkString
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret cat with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a b c | cat"
      evalCommandLine(commandLine, cli, sink)
      val expected = "a b c" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret wc with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testFilePath = getClass.getResource("/lorem_ipsum.txt").getPath
      val commandLine = s"wc $testFilePath"
      evalCommandLine(commandLine, cli, sink)
      val expected = "7 1743 12124 "
      val actual = Source.fromInputStream(source).mkString
      assert(actual.startsWith(expected))
    }
  }

  it should "interpret wc no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a b c | wc"
      evalCommandLine(commandLine, cli, sink)
      val expected = "1 3 6" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret pwd" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "pwd"
      evalCommandLine(commandLine, cli, sink)
      val expected = System.getProperty("user.dir") + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret grep with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testFilePath = getClass.getResource("/grep_test.txt").getPath
      val commandLine = s"grep 3 $testFilePath"
      evalCommandLine(commandLine, cli, sink)
      val expected = s"3${Properties.lineSeparator}3${Properties.lineSeparator}"
      val actual = Source.fromInputStream(source).mkString
      assert(actual.startsWith(expected))
    }
  }

  it should "interpret grep with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a | grep a"
      evalCommandLine(commandLine, cli, sink)
      val expected = "a" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  "grep -i" should "ignore case #1" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo A | grep -i a"
      evalCommandLine(commandLine, cli, sink)
      val expected = "A" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  "grep -i" should "ignore case #2" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a | grep -i A"
      evalCommandLine(commandLine, cli, sink)
      val expected = "a" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  "grep -w" should "match whole word #1" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo ab | grep -w ab"
      evalCommandLine(commandLine, cli, sink)
      val expected = "ab" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  "grep -w" should "match whole word #2" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo ab | grep -w a"
      evalCommandLine(commandLine, cli, sink)
      val expected = ""
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret grep -A n" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testFilePath = getClass.getResource("/grep_test.txt").getPath
      val commandLine = s"grep -A 1 3 $testFilePath"
      evalCommandLine(commandLine, cli, sink)
      val expected = s"3${Properties.lineSeparator}1${Properties.lineSeparator}" +
                     s"3${Properties.lineSeparator}1${Properties.lineSeparator}"
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret cd with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      evalCdCommand("cd", cli, sink)
      val expected = System.getProperty("user.home") + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret cd with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testDirPath = URLDecoder.decode(getClass.getResource("/test_folder").getPath, "UTF-8")
      evalCdCommand("cd " + testDirPath, cli, sink)
      val expected = testDirPath + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret ls with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      evalLsCommand("ls", cli, sink)
      val testFilePath = URLDecoder.decode(getClass
        .getResource("/test_folder/ls_command_result.txt")
        .getPath, "UTF-8")
      val expected = Source.fromFile(testFilePath).mkString
      val actual = Source.fromInputStream(source).mkString + Properties.lineSeparator
      assertResult(expected)(actual)
    }
  }

  it should "interpret ls with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      evalLsCommand("ls test_folder_2", cli, sink)
      val testFilePath = URLDecoder.decode(getClass
        .getResource("/test_folder/test_folder_2/ls_command_result.txt")
        .getPath, "UTF-8")
      val expected = Source.fromFile(testFilePath).mkString
      val actual = Source.fromInputStream(source).mkString + Properties.lineSeparator
      assertResult(expected)(actual)
    }
  }

  it should "interpret external command with args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val testFilePath = URLDecoder.decode(getClass.getResource("/lorem_ipsum.txt").getPath, "UTF-8")
      val commandLine = s"head -n 1 $testFilePath"
      evalCommandLine(commandLine, cli, sink)
      val expected = Source.fromFile(testFilePath).getLines().next() + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret external command with no args" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "echo a b c | head -n 1"
      evalCommandLine(commandLine, cli, sink)
      val expected = "a b c" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }

  it should "interpret an assignment and substitution" in withInterpreter { cli =>
    withPipe { (sink, source) =>
      val commandLine = "answer=42 | echo $answer"
      evalCommandLine(commandLine, cli, sink)
      val expected = "42" + Properties.lineSeparator
      val actual = Source.fromInputStream(source).mkString
      assertResult(expected)(actual)
    }
  }
}
