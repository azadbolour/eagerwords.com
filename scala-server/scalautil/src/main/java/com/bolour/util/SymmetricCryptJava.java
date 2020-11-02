/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.UnsupportedEncodingException;
import java.security.Key;
import java.util.Base64;

public class SymmetricCryptJava implements ISymmetricCrypt {

    private static final String TRANSFORMATION = "AES/CBC/PKCS5Padding";
    private static final String KEY_ALGORITHM = "AES"; // Yes - different from transformation.
    private static final int KEY_LENGTH = 32;
    // Keeping it simple for current needs.
    byte[] InitializationVector = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    IvParameterSpec ivSpec = new IvParameterSpec(InitializationVector);
    private Key key;

    /**
     * Create a symmetric encryptor/decryptor by using a given secret.
     *
     * It is the clint's responsibility to close the input stream when the
     * the constructor returns.
     *
     * @param secret The secret value of the key.
     */
    public SymmetricCryptJava(String secret) throws Exception {
        this.key = mkSecretKey(secret);
    }

    @Override
    public String encrypt(String plainText) throws Exception {
        Cipher cipher = getCipher(Cipher.ENCRYPT_MODE);
        byte[] bytes = plainText.getBytes("UTF-8");
        byte[] encrypted = cipher.doFinal(bytes);
        String encoded = Base64.getEncoder().encodeToString(encrypted);
        return encoded;
    }

    @Override
    public String decrypt(String cipherText) throws Exception {
        Cipher cipher = getCipher(Cipher.DECRYPT_MODE);
        byte[] encrypted = Base64.getDecoder().decode(cipherText);
        byte[] bytes = cipher.doFinal(encrypted);
        String plainText = new String(bytes);
        return plainText;
    }

    private Cipher getCipher(int mode) throws Exception {
        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(mode, this.key, this.ivSpec);
        return cipher;
    }

    private static SecretKey mkSecretKey(String secret) throws UnsupportedEncodingException {
        String padding = new String(new char[KEY_LENGTH]);
        String keyString = (secret + padding).substring(0, KEY_LENGTH);
        byte[] key = keyString.getBytes("UTF-8");
        return new SecretKeySpec(key, KEY_ALGORITHM);
    }
}

