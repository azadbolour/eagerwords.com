/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.util;

/**
 * Interface for encryption/decryption of plaintext strings
 * to cipher text strings.
 */
public interface ISymmetricCrypt {
    String encrypt(String plainText) throws Exception;
    String decrypt(String cipherText) throws Exception;
}
