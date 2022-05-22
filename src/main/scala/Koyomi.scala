package ink.sora

import java.io.File
import scopt.OParser

import java.nio.file.Files
import scala.annotation.tailrec
import scala.collection.mutable

case class KoyomiArguments(inputFile: Option[File] = None,
                           outputFile: Option[File] = None,
                           convertEbnf: Boolean = true)

enum KoyomiFlags extends Enum[KoyomiFlags]:
  case ConvertEBNF extends KoyomiFlags
end KoyomiFlags

@tailrec
def process(productions: List[Production], flags: Set[KoyomiFlags], outputFile: File): Unit =
  flags.toList.sorted match
    case KoyomiFlags.ConvertEBNF :: tail =>
      process(productions |> Instrumentation.normalize, tail.toSet, outputFile)
    case Nil =>
      val result = productions |> Instrumentation.format
      println("Process finished with result: ")
      println(result)
      Files.writeString(outputFile.toPath, result)
end process

@main
def main(args: String*): Unit =
  val builder = OParser.builder[KoyomiArguments]
  val parser = {
    import builder._
    OParser.sequence(
      programName("Koyomi"),
      head("Koyomi", "0.1.0"),
      opt[File]('i', "input")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(inputFile = Some(x)))
        .text("input file"),
      opt[File]('o', "output")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(outputFile = Some(x)))
        .text("output file"),
      opt[Unit]('e', "convert-ebnf")
        .action((_, c) => c.copy(convertEbnf = true))
        .text("convert EBNF to BNF"),
      help("help").text("prints this usage text"),
      version("version").text("prints the version")
    )
  }
  OParser.parse(parser, args, KoyomiArguments()) match {
    case Some(KoyomiArguments(inputFile, outputFile, convertEbnf)) =>
      val i = inputFile.getOrElse(throw new IllegalArgumentException("input file is required"))
      val o = outputFile.getOrElse(throw new IllegalArgumentException("output file is required"))
      val set = mutable.Set[KoyomiFlags]()
      if convertEbnf then set += KoyomiFlags.ConvertEBNF
      val parser = Parser(Scanner(Files.readString(i.toPath)))
      process(parser.parse().productions, set.toSet, o)
    case _ => System.exit(1)
  }
end main