/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import javax.mail.internet.MimeMessage;
import java.util.Properties;

public class MockSmtpMailService extends AbstractSmtpMailService implements IMailService {

    public MockSmtpMailService() throws Exception {
        Properties properties = new Properties();
        properties.setProperty(MAIL_SMTP_USER, "nobody");
        properties.setProperty(MAIL_SMTP_PASSWORD, "none");
        properties.setProperty(MAIL_SMTP_HOST, "smtp.unknown.com");
        properties.setProperty(MAIL_SMTP_AUTH, "true");
        properties.setProperty(MAIL_SMTP_ENABLE_TLS, "true");
        init(properties);
    }

    @Override
    public void sendMail(String recipientEmail, String subject, String text) throws Exception {
        MimeMessage message = buildMessage(recipientEmail, subject, text);
        // Just print it out.
        debugMessage(message);
    }
}
