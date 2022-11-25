package app.softnetwork.sbt.build

import com.typesafe.sbt.SbtGit.GitKeys
import sbt._

trait GitSupport extends ConsoleUtils {
  def prompt(state: State) = {
    val extracted = Project.extract(state)
    val git = extracted.get(GitKeys.gitReader)
    val name = extracted.get(Keys.name)
    "[" + cyan(bold(name)) + "](" + green(git.withGit(_.branch)) + ") " +
    (if (git.withGit(_.hasUncommittedChanges)) yellow("✗ ")
     else "") + "> "
  }
}
