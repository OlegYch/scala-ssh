package com.decodified.scalassh

object SSH {
  def apply[T](host: String, configProvider: HostConfigProvider = HostFileConfig())
              (body: SshClient => Result[T]): Validated[T] = {
    SshClient(host, configProvider).right.flatMap { client =>
      val result = {
        try { body(client).result }
        catch { case e: Exception => client.log.error("Exception",e); Left(e.toString) }
      }
      try {
        client.close()
        result
      }
      catch {
        case e => client.log.error("Exception",e); Left("Exception on close" + result.toString)
      }
    }
  }

  case class Result[T](result: Validated[T])

  object Result extends LowerPriorityImplicits {
    implicit def validated2Result[T](value: Validated[T]) = Result(value)
  }
  private[SSH] abstract class LowerPriorityImplicits {
    implicit def any2Result[T](value: T) = Result(Right(value))
  }
}

