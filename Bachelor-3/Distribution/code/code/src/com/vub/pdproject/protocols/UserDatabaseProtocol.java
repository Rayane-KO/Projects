// rkouidan
package com.vub.pdproject.protocols;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;
import java.security.PublicKey;

public interface UserDatabaseProtocol extends Remote {
    String register(String username, String password) throws RemoteException, GeneralSecurityException;
    String retrieve(String username) throws RemoteException, GeneralSecurityException;
    void exit() throws RemoteException;
    PublicKey getKey() throws RemoteException;
    void setSessionKey(String encryptedSessionKey) throws RemoteException, GeneralSecurityException;
}
