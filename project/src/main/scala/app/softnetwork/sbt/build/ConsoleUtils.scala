package app.softnetwork.sbt.build

import sbt.Def

trait ConsoleUtils {

  def bold(str: String): String = Def.withColor(str, Some(Console.BOLD))
  def white(str: String): String = Def.withColor(str, Some(Console.WHITE))
  def green(str: String): String = Def.withColor(str, Some(Console.GREEN))
  def red(str: String): String = Def.withColor(str, Some(Console.RED))
  def yellow(str: String): String = Def.withColor(str, Some(Console.YELLOW))
  def blue(str: String): String = Def.withColor(str, Some(Console.BLUE))
  def cyan(str: String): String = Def.withColor(str, Some(Console.CYAN))
}

object ConsoleUtils extends ConsoleUtils
