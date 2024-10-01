// rkouidan
package com.vub.pdproject.tests;

import com.vub.pdproject.UserDatabaseServer;
import org.junit.BeforeClass;
import org.junit.Test;

import javax.crypto.SecretKey;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;

import static com.vub.pdproject.utils.SecurityUtil.*;
import static org.junit.Assert.*;

public class UserDatabaseServerTest {
    private static UserDatabaseServer userDatabaseServer;
    private static SecretKey sessionKey;

    @BeforeClass
    public static void setUp() throws RemoteException, GeneralSecurityException {
        userDatabaseServer = new UserDatabaseServer();
        sessionKey = generateSessionKey();
        String encodedKey = byteToString(encryptKey(sessionKey.getEncoded(), userDatabaseServer.getKey()));
        userDatabaseServer.setSessionKey(encodedKey);
    }

    @Test
    public void testRegister() throws GeneralSecurityException, RemoteException {
        String username = encryptData("user1", sessionKey);
        String password = encryptData("password", sessionKey);
        String registered = userDatabaseServer.register(username, password);

        assertTrue(Boolean.parseBoolean(decryptData(registered, sessionKey)));
    }

    @Test
    public void testRegisterWithTakenUsername() throws GeneralSecurityException, RemoteException {
        String username = encryptData("user2", sessionKey);
        String password = encryptData("password", sessionKey);
        userDatabaseServer.register(username, password);

        String registered = userDatabaseServer.register(username, password);
        assertFalse(Boolean.parseBoolean(decryptData(registered, sessionKey)));
    }

    @Test
    public void testRetrieve() throws GeneralSecurityException, RemoteException {
        String username = encryptData("user3", sessionKey);
        String password = encryptData("password", sessionKey);
        userDatabaseServer.register(username, password);

        String result = userDatabaseServer.retrieve(username);
        String decryptedPassword = decryptData(result, sessionKey);

        assertEquals("password", decryptedPassword);
    }

    @Test
    public void testRetrieveNonExisting() throws GeneralSecurityException, RemoteException {
        String encryptedUsername = encryptData("user4", sessionKey);
        String result = userDatabaseServer.retrieve(encryptedUsername);
        String decryptedPassword = decryptData(result, sessionKey);

        assertNull(decryptedPassword);
    }
}
