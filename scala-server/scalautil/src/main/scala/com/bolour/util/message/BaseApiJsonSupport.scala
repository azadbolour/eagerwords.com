package com.bolour.util.message

import play.api.libs.json._
import play.api.libs.json.Json.{reads, writes}
import play.api.mvc.Result

import com.bolour.util.CommonUtil.Email
import com.bolour.util.controller.BaseDtoConverters.{toInternalErrorDto, toSystemOverloadedErrorDto}
import com.bolour.util.controller.ControllerHelper.unprocessableJson
import com.bolour.util.domain.BaseExceptions.{BaseException, InternalAppException, SystemOverloadedException}

/**
  * Implicit JSON readers and writers for basic app dto objects.
  */
object BaseApiJsonSupport {
  implicit def optionFormat[T: Format]: Format[Option[T]] = new Format[Option[T]]{
    override def reads(json: JsValue): JsResult[Option[T]] = json.validateOpt[T]

    override def writes(o: Option[T]): JsValue = o match {
      case Some(t) ⇒ implicitly[Writes[T]].writes(t)
      case None ⇒ JsNull
    }
  }

  implicit val charReads: Reads[Char] = new Reads[Char] {
    def reads(json: JsValue): JsResult[Char] = Json.fromJson[String](json) map { _.head }
  }

  implicit val emailReads: Reads[Email] = new Reads[Email] {
    def reads(json: JsValue): JsResult[Email] = {
      val string = Json.fromJson[String](json)
      string map (value => Email(value))
    }
  }

  /**
    * The json encoding of unit.
    *
    * Treat unit as an empty tuple which is encoded as an array of its field values.
    * The [String] tye parameter is provided to keep the compiler happy
    * (since String is implicitly convertible to json).
    */
  implicit val unitReads: Reads[Unit] = new Reads[Unit] {
    def reads(json: JsValue): JsSuccess[Unit] = {
      val result = Json.fromJson[List[String]](json)
      result.get match {
        case Nil => JsSuccess(())
        case _ => throw new IllegalArgumentException(s"unitRead: ${json.toString}")
      }
    }
  }

  // implicit val userDtoReads: Reads[UserDto] = reads[UserDto]
  implicit val handShakeResponseReads: Reads[HandShakeResponse] = reads[HandShakeResponse]

  implicit val charWrites: Writes[Char] = new Writes[Char] {
    def writes(o: Char): JsValue = Json.toJson[String](o.toString)
  }

  implicit val emailWrites: Writes[Email] = new Writes[Email] {
    def writes(o: Email): JsValue = Json.toJson[String](o.email)
  }

  implicit val unitWrites: Writes[Unit] = new Writes[Unit] {
    def writes(o: Unit): JsValue = Json.toJson(List[String]())
  }

  implicit val handShakeResponseWrites: OWrites[HandShakeResponse] = writes[HandShakeResponse]

  implicit val systemOverloadedErrorDtoWrites: OWrites[SystemOverloadedErrorDto] = writes[SystemOverloadedErrorDto]
  implicit val internalErrorDtoWrites: OWrites[InternalErrorDto] = writes[InternalErrorDto]

  def baseExceptionToUnprocessableEntity(ex: BaseException): Result = {
    ex match {
      case ex: InternalAppException => unprocessableJson(toInternalErrorDto(ex))
      case ex: SystemOverloadedException => unprocessableJson(toSystemOverloadedErrorDto(ex))
    }
  }
}
