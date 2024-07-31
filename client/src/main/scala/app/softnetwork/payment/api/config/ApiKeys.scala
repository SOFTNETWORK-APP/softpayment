package app.softnetwork.payment.api.config

import java.nio.file.Paths

object ApiKeys {

  private[this] lazy val filePath: String = {
    Paths.get(SoftPayClientSettings.SP_CONFIG).toFile.mkdirs()
    SoftPayClientSettings.SP_CONFIG + "/apiKeys.conf"
  }

  private[this] def write(apiKeys: Map[String, String]): Unit = {
    val file = Paths.get(filePath).toFile
    file.createNewFile()
    val apiKeysWriter = new java.io.BufferedWriter(new java.io.FileWriter(file))
    apiKeys.foreach { case (clientId, apiKey) =>
      apiKeysWriter.append(
        s"""$clientId=$apiKey
           |""".stripMargin
      )
    }
    apiKeysWriter.close()
  }

  def list(): Map[String, String] = {
    val file = Paths.get(filePath).toFile
    file.createNewFile()
    import scala.io.Source
    val source = Source
      .fromFile(filePath)
    val apiKeys =
      source.getLines
        .filter(line => line.nonEmpty)
        .map(line => line.split("="))
        .map { case Array(clientId, apiKey) => clientId.trim -> apiKey.trim }
        .toMap
    source.close()
    apiKeys //.keys.map(clientId => ApiKey(clientId, Some(apiKeys(clientId)))).toSeq
  }

  def +(clientId: String, apiKey: String): Unit = write(list() + (clientId -> apiKey))

  def -(clientId: String): Unit = write(list() - clientId)

  def get(clientId: String): Option[String] = list().get(clientId)

}
