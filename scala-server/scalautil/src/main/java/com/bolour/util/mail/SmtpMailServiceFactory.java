package com.bolour.util.mail;

import com.typesafe.config.Config;

public class SmtpMailServiceFactory {
    public static final String MOCK_EMAIL_KEY = "mock";

    public static IMailService create(Config conf) throws Exception {
        boolean isMock = conf.getBoolean(MOCK_EMAIL_KEY);
        return isMock ? new MockSmtpMailService(conf) : new SmtpMailService(conf);
    }

    public static IMailService create(Config conf, String user, String password) throws Exception {
        boolean isMock = conf.getBoolean(MOCK_EMAIL_KEY);
        return isMock ? new MockSmtpMailService(conf, user, password) : new SmtpMailService(conf, user, password);
    }
}
