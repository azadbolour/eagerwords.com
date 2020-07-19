package com.bolour.util

import collection.JavaConverters._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

class SymmetricCryptSpec extends FlatSpec with Matchers {

  val logger = LoggerFactory.getLogger(this.getClass)

  // val alias = "testkey"
  val secret = "secret"
  // val password = "12345678" // Double-duty for keystore and for key.

  val plainText = "some text to encrypt"

  "symmetric crypt" should "encrypt and decrypt" in {
    val crypter = new SymmetricCryptJava(secret)
    val cipherText = crypter.encrypt(plainText)
    logger.info(s"encrypted: ${cipherText}")
    val decodedPlainText = crypter.decrypt(cipherText);
    logger.info(s"decoded: ${decodedPlainText}")
    decodedPlainText shouldBe plainText
  }

}
