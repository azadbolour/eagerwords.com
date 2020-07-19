/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util.mail;

import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import java.util.Properties;

public class AbstractSmtpMailService implements IMailService {

    // TODO. Constants for property names.
    // TODO. Add logging.

    protected static String MAIL_SMTP_USER = "mail.smtp.user";
    protected static String MAIL_SMTP_PASSWORD = "mail.smtp.password";
    protected static String MAIL_SMTP_HOST = "mail.smtp.host";
    protected static String MAIL_SMTP_ENABLE_TLS = "mail.smtp.starttls.enable";
    protected static String MAIL_SMTP_AUTH = "mail.smtp.auth";
    protected static String MAIL_PROTOCOL = "smtp";

    protected Properties properties;
    protected Session session;
    protected String senderEmail;

    protected void init(Properties properties) throws Exception {
        this.properties = properties;
        String user = properties.getProperty(MAIL_SMTP_USER);
        this.senderEmail = user;
        String password = properties.getProperty(MAIL_SMTP_PASSWORD);
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

