/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package controllers

import org.slf4j.LoggerFactory
import play.api.libs.json.{JsError, JsPath, JsResult, JsValue, JsonValidationError, Reads}
import play.api.mvc.{AbstractController, ControllerComponents, Request, Result}

class ControllerBase(cc: ControllerComponents) extends AbstractController(cc) {

  private val logger = LoggerFactory.getLogger(this.getClass)

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


}
