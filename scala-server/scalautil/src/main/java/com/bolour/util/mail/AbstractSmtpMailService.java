/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import com.typesafe.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import java.util.Properties;

public class AbstractSmtpMailService implements IMailService {

    // TODO. Constants for property names.
    // TODO. Add logging.

    static final String MAIL_SMTP_USER = "mail.smtp.user";
    protected static final String MAIL_SMTP_PASSWORD = "mail.smtp.password";
    protected static final String MAIL_SMTP_HOST = "mail.smtp.host";
    protected static final String MAIL_SMTP_PORT = "mail.smtp.port";
    protected static final String MAIL_SMTP_STARTTLS_ENABLE = "mail.smtp.starttls.enable";
    protected static final String MAIL_SMTP_AUTH = "mail.smtp.auth";
    protected static final String MAIL_PROTOCOL = "smtp";

    protected Properties properties;
    protected Session session;
    protected String senderEmail;

    private static Logger logger = LoggerFactory.getLogger(AbstractSmtpMailService.class);

    /**
     * Initialize with a typesafe config tree representing mail smtp parameters
     * whose paths are consistent with the standard smtp property names.
     *
     * @param conf Includes a tree rooted at the path "mail.smtp" that
     *             contains all the needed mail smtp parameters at path
     *             locations conforming to their standard naming conventions.
     */
    protected void init(Config conf) throws Exception {
        String user = conf.getString(MAIL_SMTP_USER);
        String password = conf.getString(MAIL_SMTP_PASSWORD);
        init(conf, user, password);
    }

    protected void init(Config conf, String user, String password) throws Exception {
        Properties properties = new Properties();
        properties.setProperty(MAIL_SMTP_USER, user);
        properties.setProperty(MAIL_SMTP_PASSWORD, password);
        String host = conf.getString(MAIL_SMTP_HOST);
        logger.info("email host: " + host);
        properties.setProperty(MAIL_SMTP_HOST, host);
        properties.setProperty(MAIL_SMTP_PORT, conf.getString(MAIL_SMTP_PORT));
        properties.setProperty(MAIL_SMTP_STARTTLS_ENABLE, conf.getString(MAIL_SMTP_STARTTLS_ENABLE));
        properties.setProperty(MAIL_SMTP_AUTH, conf.getString(MAIL_SMTP_AUTH));
        init(properties, user, password);
    }

    private void init(Properties properties, String user, String password) throws Exception {
        this.properties = properties;
        this.senderEmail = user;
        this.session = Session.getInstance(properties, new Authenticator() {
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(user, password);
            }
        });
    }

    protected MimeMessage buildMessage(String recipientEmail, String subject, String text) throws Exception {
        MimeMessage message = new MimeMessage(this.session);
        message.setFrom(new InternetAddress(this.senderEmail));
        message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(recipientEmail));
        message.setSubject(subject);
        MimeBodyPart mimeBodyPart = new MimeBodyPart();
        mimeBodyPart.setContent(text, "text/html; charset=utf-8");
        Multipart multiPart = new MimeMultipart("alternative");
        multiPart.addBodyPart(mimeBodyPart);
        message.setContent(multiPart);
        return message;
    }

    public static String EMAIL_FROM_LABEL = "from: ";
    public static String EMAIL_RECIPIENT_LABEL = "recipient: ";
    public static String EMAIL_SUBJECT_LABEL = "subject: ";
    public static String EMAIL_CONTENT_LABEL = "content: ";

    protected static void debugMessage(MimeMessage message) throws Exception {
        String recipient = message.getRecipients(Message.RecipientType.TO)[0].toString();
        String from = message.getFrom()[0].toString();
        String subject = message.getSubject();
        MimeBodyPart part = (MimeBodyPart) (((Multipart) message.getContent()).getBodyPart(0));
        String content = part.getContent().toString();

        System.out.println(EMAIL_SUBJECT_LABEL + subject);
        System.out.println(EMAIL_FROM_LABEL + from);
        System.out.println(EMAIL_RECIPIENT_LABEL + recipient);
        System.out.println(EMAIL_CONTENT_LABEL + content);

        // BEWARE! PROPERTIES INCLUDES CREDENTIALS. FOR DEBUGGING ONLY.
        // properties.list(System.out);
    }

    @Override
    public void sendMail(String recipientEmail, String subject, String text) throws Exception {

    }
}

