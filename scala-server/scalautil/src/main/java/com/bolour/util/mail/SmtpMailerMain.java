package com.bolour.util.mail;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import java.io.Console;

/**
 * Main program for testing email transmission.
 *
 * Add mail smtp properties to application.conf and override with
 * corresponding environment variables. Credentials are obtained here.
 * Credentials from application.conf are ignored.
 */
public class SmtpMailerMain {
    public static void main(String[] args) throws Exception {
        Console console = System.console();
        if (args.length < 3) {
            console.printf("usage: SmtpMailerMain email subject text");
            System.exit(-1);
        }
        String email = args[0];
        String subject = args[1];
        String text = args[2];
        console.printf("user: ");
        String user = console.readLine();
        console.printf("password: ");
        char[] passwordChars = console.readPassword();
        String password = new String(passwordChars);
        // console.printf("%s\n", user);
        java.util.Arrays.fill(passwordChars, ' ');
        Config conf = ConfigFactory.load();
        Config mailConfig = conf.getConfig("service.email");
        IMailService service = SmtpMailServiceFactory.create(mailConfig, user, password);
        service.sendMail(email, subject, text);
    }
}
