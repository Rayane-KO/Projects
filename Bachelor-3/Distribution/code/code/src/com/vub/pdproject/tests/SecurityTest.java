// rkouidan
package com.vub.pdproject.tests;

import com.google.gson.Gson;
import com.vub.pdproject.DataPeer;
import org.junit.BeforeClass;
import org.junit.Test;

import javax.crypto.SecretKey;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;
import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;

import static com.vub.pdproject.utils.SecurityUtil.*;
import static org.junit.Assert.assertEquals;

public class SecurityTest {
    private static PublicKey publicKey;
    private static PrivateKey privateKey;
    private static SecretKey sessionKey;

    @BeforeClass
    public static void setUp() throws RemoteException, GeneralSecurityException {
        KeyPair keyPair = generateKeyPair();
        publicKey = keyPair.getPublic();
        privateKey = keyPair.getPrivate();
        sessionKey = generateSessionKey();
    }

    @Test
    public void testEncryption() throws GeneralSecurityException {
        String data = "test data";
        String encryptedData = encryptData(data, publicKey);
        String decryptedData = decryptData(encryptedData, privateKey);
        assertEquals(data, decryptedData);
    }

    @Test
    public void testEncryptionSessionKey() throws GeneralSecurityException {
        String data = "test data";
        String encryptedData = encryptData(data, sessionKey);
        String decryptedData = decryptData(encryptedData, sessionKey);
        assertEquals(data, decryptedData);
    }
}
