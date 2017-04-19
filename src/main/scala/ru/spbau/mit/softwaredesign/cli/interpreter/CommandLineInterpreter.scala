package ru.spbau.mit.softwaredesign.cli.interpreter

import java.io.{File, _}
import java.net.URLDecoder
import java.nio.channels._

import org.apache.commons.io.IOUtils
import resource._
import ru.spbau.mit.softwaredesign.cli.parser.{Unit => _, _}
import scopt.OptionParser

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._
import scala.util.Properties

/** Stores environment and performs command interpretation */
class CommandLineInterpreter {
  private var workDirectory = System.getProperty("user.dir")
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
          case "grep" => evalGrep(args, in, out)
          case "cd" => evalCd(args, out)
          case "ls" => evalLs(args, out)
          case "exit" => evalExit()
          case commandName => evalExternal(commandName, args, in, out)
        }
      case _ => new IllegalStateException()
    }
  }

  private def evalCat(args: List[Block], in: InputStream, out: OutputStream): Unit = {
    if (args.isEmpty) {
      IOUtils.copy(in, out)
    } else {
      args.foreach { arg =>
        val fileName = processBlock(arg)
        for (fileIn <- managed(new FileInputStream(getAbsolutePath(fileName)))) {
          IOUtils.copy(fileIn, out)
        }
      }
    }
  }

  private def evalEcho(args: List[Block], out: OutputStream): Unit = {
    val concatArgs = args.map {
      processBlock
    } mkString " "
    out.write((concatArgs + Properties.lineSeparator).getBytes)
  }

  private def evalWc(args: List[Block], in: InputStream, out: OutputStream): Unit = {
    def countLinesWordsBytes(text: String): (Int, Int, Int) = {
      val lines = text.split(Properties.lineSeparator).length
      val words = text.split("""\s+""").length
      val bytes = text.getBytes.length
      (lines, words, bytes)
    }

    if (args.isEmpty) {
      val source = Source.fromInputStream(in).mkString
      val (lines, words, bytes) = countLinesWordsBytes(source)
      out.write(s"$lines $words $bytes${Properties.lineSeparator}".getBytes)
    } else {
      args.foreach { arg =>
        val fileName = processBlock(arg)
        for (fileIn <- managed(new FileInputStream(getAbsolutePath(fileName)))) {
          val source = Source.fromInputStream(fileIn).mkString
          val (lines, words, bytes) = countLinesWordsBytes(source)
          out.write(s"$lines $words $bytes $fileName${Properties.lineSeparator}".getBytes)
        }
      }
    }
  }

  private def evalPwd(out: OutputStream): Unit = {
    out.write((workDirectory + Properties.lineSeparator).getBytes)
  }

  private def evalGrep(args: List[Block], in: InputStream, out: OutputStream): Unit = {
    case class Config(ignoreCase: Boolean = false,
                      wordRegexp: Boolean = false,
                      afterContext: Int = 0,
                      pattern: String = "",
                      fileNames: Seq[String] = Seq())

    val parser = new OptionParser[Config]("grep") {
      head("grep", "0.99")

      opt[Unit]('i', "ignore-case").action((_, config) =>
        config.copy(ignoreCase = true)).text("Ignore case distinctions in both the <pattern> and the input files.")

      opt[Unit]('w', "word_regexp").action((_, config) =>
        config.copy(wordRegexp = true)).text("Select only those lines containing matches that form whole words.")

      opt[Int]('A', "after-context").action((value, config) =>
        config.copy(afterContext = value)).text("Print <value> lines of trailing context after matching lines.")

      arg[String]("<pattern>").required().action((regex, config) =>
        config.copy(pattern = regex)).text("Use regex as the pattern.")

      arg[String]("<file>...").unbounded().optional().action((fileName, config) =>
        config.copy(fileNames = config.fileNames :+ fileName)).text("Obtain patterns from files, one per line.")
    }

    parser.parse(args.map {
      processBlock
    }, Config()) match {
      case Some(Config(ignoreCase, wordRegexp, afterContext, pattern, fileNames)) =>
        var regexBuilder = pattern
        if (ignoreCase)
          regexBuilder = "(?i)" + regexBuilder
        if (wordRegexp)
          regexBuilder = "\\b" + regexBuilder + "\\b"
        val regex = regexBuilder.r

        def containsPattern(line: String): Boolean = {
          regex.findFirstIn(line) match {
            case Some(_) => true
            case None => false
          }
        }

        def handleInput(in: InputStream): Unit = {
          var restAfterContext = 0
          Source.fromInputStream(in).getLines.foreach { line =>
            if (containsPattern(line)) {
              out.write((line + Properties.lineSeparator).getBytes)
              restAfterContext = afterContext
            } else if (restAfterContext > 0) {
              out.write((line + Properties.lineSeparator).getBytes)
              restAfterContext -= 1
            }
          }
        }

        if (fileNames.isEmpty) {
          handleInput(in)
        } else {
          fileNames.foreach { fileName =>
            for (fileIn <- managed(new FileInputStream(getAbsolutePath(fileName)))) {
              handleInput(fileIn)
            }
          }
        }
      case None =>
    }
  }

  private def evalCd(args: List[Block], out: OutputStream): Unit = {
    if (args.isEmpty) {
      workDirectory = System.getProperty("user.home")
    } else {
      val dirName = processBlock(args.head)
      val dir = new File(getAbsolutePath(dirName))
      if (dir.exists() && dir.isDirectory) {
        workDirectory = dir.getCanonicalPath
      } else {
        throw new IOException("Нет такого каталога " + getAbsolutePath(dirName))
      }
    }
  }

  private def evalLs(args: List[Block], out: OutputStream): Unit = {
    var destName = workDirectory
    if (args.nonEmpty) {
      destName = processBlock(args.head)
      destName = getAbsolutePath(destName)
    }
    val destination = new File(destName)
    if (destination.exists()) {
      if (destination.isDirectory) {
        val notHiddenFiles = destination.listFiles().filterNot(_.isHidden)
        notHiddenFiles.foreach { file =>
          out.write((file.getName + Properties.lineSeparator).getBytes)
        }
      } else if (destination.isFile) {
        out.write((destination.getName + Properties.lineSeparator).getBytes())
      }
    } else {
      throw new IOException(destName + ": Нет такого файла или каталога")
    }
  }

  private def evalExit(): Unit = {
    System.exit(0)
  }

  private def evalExternal(commandName: String, args: List[Block], in: InputStream, out: OutputStream): Unit = {
    val concatArgs = args.map {
      processBlock
    } mkString " "
    val logger = ProcessLogger(line => out.write((line + Properties.lineSeparator).getBytes), _ => ())
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

  private def getAbsolutePath(fileName: String): String = {
    val decodedFileName = URLDecoder.decode(fileName, "UTF-8")
    if (decodedFileName.startsWith(workDirectory)) {
      decodedFileName
    } else {
      workDirectory + "/" + decodedFileName
    }
  }
}