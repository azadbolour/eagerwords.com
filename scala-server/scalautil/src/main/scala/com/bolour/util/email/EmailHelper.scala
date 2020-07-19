/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.email

import com.bolour.util.mail.AbstractSmtpMailService.EMAIL_CONTENT_LABEL

object EmailHelper {
  /**
    * Extract the authentication token from a mock confirmation email used
    * in a sign-up or login sequence.
    * @param emailString The entire mock email as a string.
    * @return The body of the email - which includes just the token.
    */
  def mockTokenExtractor(emailString: String): String = emailString.lines.find(
    line => line.startsWith(EMAIL_CONTENT_LABEL)).get.drop(EMAIL_CONTENT_LABEL.length
  )

}
