/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

/**
 * Allows a well-known user to send emails to recipients.
 *
 * The information about the user and the mail service could be obtained
 * from a properties file.
 */
public interface IMailService {
    /**
     * Send an email to a recipient.
     */
    void sendMail(String recipientEmail, String subject, String text) throws Exception;
}
