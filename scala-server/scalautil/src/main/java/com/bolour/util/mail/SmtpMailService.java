/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import com.typesafe.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.mail.Transport;
import javax.mail.event.ConnectionEvent;
import javax.mail.event.ConnectionListener;
import javax.mail.internet.MimeMessage;

public class SmtpMailService extends AbstractSmtpMailService implements IMailService {

    private static Logger logger = LoggerFactory.getLogger(SmtpMailService.class);

    private Transport transport;

    // TODO. Keep the transport connected and reuse for successive sends.

    public SmtpMailService(Config conf) throws Exception {
        init(conf);
        addTransport();
    }

    public SmtpMailService(Config conf, String user, String password) throws Exception {
        init(conf, user, password);
        addTransport();
    }

    private void addTransport() throws Exception {
        this.transport = session.getTransport(MAIL_PROTOCOL);
        ConnectionListener reconnectingListener = new ConnectionListener() {
            @Override
            public void opened(ConnectionEvent e) { }

            @Override
            public void disconnected(ConnectionEvent e) {
                // TODO. Reconnect transport - once it is continuously connected.
                // Is a disconnected transport closed? If not need to close.
            }

            @Override
            public void closed(ConnectionEvent e) { }
        };
        this.transport.addConnectionListener(reconnectingListener);
    }

    @Override
    public void sendMail(String recipientEmail, String subject, String text) throws Exception {
        String password = properties.getProperty(MAIL_SMTP_PASSWORD);
        MimeMessage message = buildMessage(recipientEmail, subject, text);
        String mailHost = properties.getProperty(MAIL_SMTP_HOST);
        try {
            this.transport.connect(mailHost, this.senderEmail, password);
            this.transport.sendMessage(message, message.getAllRecipients());
        } finally {
            try {
                this.transport.close();
            } catch (Exception ex) {
                logger.error("error in closing SMTP email transport", ex);
            }
        }
    }
}
