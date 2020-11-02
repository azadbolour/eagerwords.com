package com.bolour.util

import java.io.{File, FileInputStream, FileNotFoundException}
import java.util.UUID
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import com.bolour.util.CommonUtil.javaListToScala

object MiscUtil {
  val logger = LoggerFactory.getLogger(this.getClass)

  def stringId() = UUID.randomUUID().toString

  def readConfigStringList(path: String): Try[List[String]] = {
    Try {
      val config = ConfigFactory.load()
      val javaList = config.getStringList(path)
      javaListToScala(javaList)
    }
  }

  def mkFileSource(path: String): Try[BufferedSource] = {
    logger.info(s"mkFileSource: $path")
    val triedInput = Try {new FileInputStream(path)}
    triedInput.map(inputStream => Source.fromInputStream(inputStream))
  }

  def mkResourceSource(path: String, classLoader: ClassLoader): Try[BufferedSource] = {
    logger.info(s"mkResourceSource: $path")
    val maybeInput = Option(classLoader.getResourceAsStream(path))
    maybeInput match {
      case None => Failure(new FileNotFoundException(path))
      case Some(inputStream) => Success(Source.fromInputStream(inputStream))
    }
  }

  def readFileAsBytes(path: String): Try[Array[Byte]] = {
    var buffer: Array[Byte] = new Array[Byte](1000)
    Try {
      val file = new File(path)
      val size = file.length().toInt
      val bytes: Array[Byte] = new Array[Byte](size)
      var i = 0
      var n = 0
      val input = new FileInputStream(path)
      while (n != -1) {
        n = input.read(buffer);
        if (n != -1) {
          Array.copy(buffer, 0, bytes, i, n)
          i = i + n
        }
      }
      bytes
    }
  }

}
