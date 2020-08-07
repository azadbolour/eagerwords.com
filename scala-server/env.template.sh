#!/bin/sh

DB_NAME=$1

echo -n "db password: "; stty -echo; read DB_PASS; stty echo

export DB_HOST="eagerwords-test.cylyfz7oaqi7.us-west-1.rds.amazonaws.com"
export DB_PORT=5432
export DB_USER=postgres
export DB_PASS
export DB_NAME
export DB_TYPE=postgres 

echo -n "mail smtp password: "; stty -echo; read MAIL_SMTP_PASSWORD; stty echo

export MAIL_SMTP_HOST=smtp.provider.com
export MAIL_SMTP_PORT=587
export MAIL_SMTP_USER=user@company.com
export MAIL_SMTP_PASSWORD
export MOCK_EMAIL=false

echo -n "encryption key: "; stty -echo; read ENCRYPTION_KEY; stty echo
export ENCRYPTION_KEY

echo -n "play secret: "; stty -echo; read PLAY_SECRET; stty echo
export PLAY_SECRET
