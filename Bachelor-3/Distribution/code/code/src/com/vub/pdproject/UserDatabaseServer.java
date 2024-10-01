// rkouidan
package com.vub.pdproject;

import com.vub.pdproject.protocols.UserDatabaseProtocol;
import com.vub.pdproject.utils.SecurityUtil;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.security.*;
import java.util.concurrent.ConcurrentHashMap;

import static com.vub.pdproject.utils.SecurityUtil.*;

/**
 * UserDatabaseServer keeps track of the information needed for authenticating users
 */
public class UserDatabaseServer implements UserDatabaseProtocol {
    // Stores the users credentials (username, password)
    final ConcurrentHashMap<String, String> credentials;
    private PublicKey publicKey;
    private PrivateKey privateKey;
    private SecretKey sessionKey;

    public UserDatabaseServer() throws NoSuchAlgorithmException {
        this.credentials = new ConcurrentHashMap<>();
        KeyPair keyPair = SecurityUtil.generateKeyPair();
        publicKey = keyPair.getPublic();
        privateKey = keyPair.getPrivate();
    }

    /**
     * Returns the public key of the user database server
     */
    @Override
    public PublicKey getKey() throws RemoteException {
        return publicKey;
    }

    /**
     * Method to exchange the session key with Wikipedia server
     * @param encryptedSessionKey The encrypted session key
     */
    @Override
    public void setSessionKey(String encryptedSessionKey) throws GeneralSecurityException {
        byte[] decodedKey = stringToByte(encryptedSessionKey);
        byte[] decryptedKey = SecurityUtil.decryptKey(decodedKey, privateKey);
        this.sessionKey = new SecretKeySpec(decryptedKey,"AES");
    }

    /**
     * Registers a user in the database
     * @param username The username of the user
     * @param password The password of the user
     */
    @Override
    public synchronized String register(String username, String password) throws RemoteException, GeneralSecurityException {
        // Decrypt information
        String decryptedUsername = decryptData(username, sessionKey);
        String decryptedPassword = decryptData(password, sessionKey);
        boolean success = !credentials.containsKey(decryptedUsername);
        if (success){
            credentials.put(decryptedUsername, decryptedPassword);
        }
        return encryptData(String.valueOf(success), sessionKey);
    }

    /**
     * Retrieves the password of a user
     * @param username The username of the user
     */
    @Override
    public String retrieve(String username) throws RemoteException, GeneralSecurityException {
        // Decrypt information
        String decryptedUsername = decryptData(username, sessionKey);
        return encryptData(credentials.get(decryptedUsername), sessionKey);
    }

    /**
     * Ensures that the use database can no longer get remote calls
     */
    public void exit() throws NoSuchObjectException {
        UnicastRemoteObject.unexportObject(this, true);
    }
}
