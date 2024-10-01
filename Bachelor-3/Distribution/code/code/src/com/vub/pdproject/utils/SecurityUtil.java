// rkouidan
package com.vub.pdproject.utils;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.security.*;
import java.util.Base64;

/**
 * Contains all the necessary function for key generation and encryption/decryption
 * sources:
 * https://stackoverflow.com/questions/25351717/how-to-implement-password-based-hybrid-encryption-in-java
 */
public interface SecurityUtil {
    String nullCode = "__NULL__";

    /**
     * Generates a keypair of public and private key
     */
    static KeyPair generateKeyPair() throws NoSuchAlgorithmException {
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048);
        return keyPairGenerator.genKeyPair();
    }

    /**
     * Generates a session key
     */
    static SecretKey generateSessionKey() throws NoSuchAlgorithmException {
        KeyGenerator keyGenerator = KeyGenerator.getInstance("AES");
        keyGenerator.init(256);
        return keyGenerator.generateKey();
    }

    /**
     * Encrypts data using a given key
     * @param data Data to be encrypted
     * @param key The key to encrypt the data with
     */
    static String encryptData(String data, Key key)
            throws GeneralSecurityException {
        if (data != null){
            byte[] encodedData = data.getBytes();
            Cipher cipher;
            if (key.getAlgorithm().equals("AES")){
                cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
            } else {
                cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding");
            }
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] encryptedData = cipher.doFinal(encodedData);
            return Base64.getEncoder().encodeToString(encryptedData);
        }
        return nullCode;
    }

    /**
     * Encrypts a key with another given key
     * @param keyToEncrypt Is the key to be encrypted
     * @param key Is the key to encrypt the data with
     */
    static byte[] encryptKey(byte[] keyToEncrypt, PublicKey key)
            throws GeneralSecurityException {
        Cipher cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding");
        cipher.init(Cipher.ENCRYPT_MODE, key);
        return cipher.doFinal(keyToEncrypt);
    }

    /**
     * Decrypts data using a given key
     * @param data Data to be decrypted
     * @param key The key to decrypt the data with
     */
    static String decryptData(String data, Key key)
            throws GeneralSecurityException {
        if (!data.equals(nullCode)){
            byte[] decodedData = Base64.getDecoder().decode(data);
            Cipher deCipher;
            if (key.getAlgorithm().equals("AES")){
                deCipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
            } else {
                deCipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding");
            }
            deCipher.init(Cipher.DECRYPT_MODE, key);
            return new String(deCipher.doFinal(decodedData));
        }
        return null;
    }

    /**
     * Decrypts a key with another given key
     * @param keyToDecrypt Is the key to be decrypted
     * @param key Is the key to decrypt the data with
     */
    static byte[] decryptKey(byte[] keyToDecrypt, PrivateKey key)
            throws GeneralSecurityException {
        Cipher deCipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding");
        deCipher.init(Cipher.DECRYPT_MODE, key);
        return deCipher.doFinal(keyToDecrypt);
    }

    /**
     * Converts an array of bytes to a string
     * @param data The array of bytes to convert
     */
    static String byteToString(byte[] data) {
        return Base64.getEncoder().encodeToString(data);
    }

    /**
     * Converts a string to an array of bytes
     * @param data The string to convert
     */
    static byte[] stringToByte(String data) {
        return Base64.getDecoder().decode(data);
    }
}
