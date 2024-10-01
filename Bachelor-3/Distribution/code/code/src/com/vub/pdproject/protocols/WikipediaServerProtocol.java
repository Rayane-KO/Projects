// rkouidan
package com.vub.pdproject.protocols;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;
import java.security.PublicKey;

public interface WikipediaServerProtocol extends Remote {
    void register(String username, String password, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    String login(String username, String password, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    boolean get(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    void add(String title, String content, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    String startEdit(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    void edit(String title, String content, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    void endEdit(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    void showHistory(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException;
    void exit() throws RemoteException;
    PublicKey getKey() throws RemoteException;
    void setSessionKey(String sessionKey, ClientProtocol client) throws RemoteException, GeneralSecurityException;
}
