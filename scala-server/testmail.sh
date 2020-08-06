#!/bin/sh

# Test sending and email to an email address given as its only argument.
# Will prompt for sender and password to the SMTP service.
# Expects other SMTP email properties to be set in application.conf 
# (possibly via env variables).

email=$1
subject=testing
text=testing

mailerClass=com.bolour.util.mail.SmtpMailerMain

sbt "runMain $mailerClass $email $subject $text"

