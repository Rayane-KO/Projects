// rkouidan
package com.vub.pdproject.protocols;

import com.vub.pdproject.data.models.Article;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;
import java.security.PublicKey;

public interface DataPeerProtocol extends Remote{
    String isFull() throws RemoteException, GeneralSecurityException;
    String get(String name) throws RemoteException, GeneralSecurityException;
    void add(String title, String content) throws RemoteException, GeneralSecurityException;
    void edit(String title, String content) throws RemoteException, GeneralSecurityException;
    String showHistory(String title) throws RemoteException, GeneralSecurityException;
    void exit() throws RemoteException;
    PublicKey getKey() throws RemoteException;
    void setSessionKey(String encryptedSessionKey) throws RemoteException, GeneralSecurityException;
    String getLoad() throws RemoteException, GeneralSecurityException;
    String getCapacity() throws RemoteException, GeneralSecurityException;
}
