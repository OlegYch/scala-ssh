package com.decodified.scalassh

import net.schmizz.sshj.connection.channel.direct.Session
import java.io.{FileInputStream, File, ByteArrayInputStream, InputStream}
import java.util.concurrent.TimeUnit

case class Command(command: String, input: CommandInput = CommandInput.NoInput, timeout: Option[Int] = None)

object Command {
  implicit def string2Command(cmd: String) = Command(cmd)
}

case class CommandInput(inputStream: Option[InputStream])

object CommandInput {
  lazy val NoInput = CommandInput(None)
  implicit def apply(input: String, charsetName: String = "UTF8"): CommandInput = apply(input.getBytes(charsetName))
  implicit def apply(input: Array[Byte]): CommandInput = apply(Some(new ByteArrayInputStream(input)))
  implicit def apply(input: InputStream): CommandInput = apply(Some(input))
  def fromFile(file: String): CommandInput = fromFile(new File(file))
  def fromFile(file: File): CommandInput = new FileInputStream(file)
  def fromResource(resource: String): CommandInput = getClass.getClassLoader.getResourceAsStream(resource)
}

class CommandResult(val channel: Session.Command, val timeout: Option[Int]) {
  def stdErrStream: InputStream = channel.getErrorStream
  def stdOutStream: InputStream = channel.getInputStream
  lazy val stdErrBytes = new StreamCopier().emptyToByteArray(join.stdErrStream)
  lazy val stdOutBytes = new StreamCopier().emptyToByteArray(join.stdOutStream)
  def stdErrAsString(charsetname: String = "utf8") = new String(stdErrBytes, charsetname)
  def stdOutAsString(charsetname: String = "utf8") = new String(stdOutBytes, charsetname)
  lazy val exitSignal: Option[String] = Option(join.channel.getExitSignal).map(_.toString)
  lazy val exitCode: Option[Int] = Option(join.channel.getExitStatus)
  lazy val exitErrorMessage: Option[String] = Option(join.channel.getExitErrorMessage)
  lazy val join: CommandResult = {
    (timeout) match {
      case Some(timeout) => channel.join(timeout, TimeUnit.MILLISECONDS)
      case None => channel.join()
    }
    this
  }
}