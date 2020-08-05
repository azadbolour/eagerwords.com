/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import javax.mail.*;
import javax.mail.internet.*;

import com.typesafe.config.Config;

public class SmtpMailService extends AbstractSmtpMailService implements IMailService {

    // TODO. Add logging.

    public SmtpMailService(Config conf) throws Exception {
        init(conf);
    }

    public SmtpMailService(Config conf, String user, String password) throws Exception {
        init(conf, user, password);
    }

    @Override
    public void sendMail(String recipientEmail, String subject, String text) throws Exception {
        String password = properties.getProperty(MAIL_SMTP_PASSWORD);
        MimeMessage message = buildMessage(recipientEmail, subject, text);
        Transport transport = session.getTransport(MAIL_PROTOCOL);
        String mailHost = properties.getProperty(MAIL_SMTP_HOST);
        transport.connect(mailHost, this.senderEmail, password);
        transport.sendMessage(message, message.getAllRecipients());
    }
}
