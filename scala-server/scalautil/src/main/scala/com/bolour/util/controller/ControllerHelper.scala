package com.bolour.util.controller

import play.api.libs.json._
import play.api.mvc.{Request, Result}
import play.api.mvc.Results._

object ControllerHelper {
  /** Shorthand for validation errors type defined in JsError. */
  type ValidationErrors = Seq[(JsPath, Seq[JsonValidationError])]

  /**
   * Shorthand for calling the Play validator.
   */
  def validate[Body](request: Request[JsValue])(implicit reads: Reads[Body]): JsResult[Body] = request.body.validate[Body]

  /**
   * Create a bad request response from a set of errors.
   */
  def badRequest: ValidationErrors => Result = (errors: ValidationErrors) => BadRequest(JsError.toJson(errors))

  def unprocessableJson[DTO](dto: DTO)(implicit writes: Writes[DTO]): Result = UnprocessableEntity(Json.toJson(dto))

}
