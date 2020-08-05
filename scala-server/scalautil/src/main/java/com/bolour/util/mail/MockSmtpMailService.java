/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import javax.mail.internet.MimeMessage;
import java.util.Properties;
import com.typesafe.config.Config;

public class MockSmtpMailService extends AbstractSmtpMailService implements IMailService {

    public MockSmtpMailService(Config conf) throws Exception {
        init(conf);
    }

    public MockSmtpMailService(Config conf, String user, String password) throws Exception {
        init(conf, user, password);
    }

    @Override
    public void sendMail(String recipientEmail, String subject, String text) throws Exception {
        MimeMessage message = buildMessage(recipientEmail, subject, text);
        // Just print it out.
        debugMessage(message);
    }
}
