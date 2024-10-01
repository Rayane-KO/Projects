// rkouidan
package com.vub.pdproject.protocols;

import com.vub.pdproject.data.models.Article;

import java.io.IOException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;
import java.security.PublicKey;

public interface ClientProtocol extends Remote {
    void register(String username, String password) throws RemoteException, GeneralSecurityException;
    void login(String username, String password) throws RemoteException, GeneralSecurityException;
    void getMessage(String message) throws RemoteException, GeneralSecurityException;
    void get(String title) throws RemoteException, GeneralSecurityException;
    void add(String title, String content) throws RemoteException, GeneralSecurityException;
    boolean startEdit(String title) throws RemoteException, GeneralSecurityException;
    void edit(String title, String content) throws RemoteException, GeneralSecurityException;
    void endEdit(String title) throws RemoteException, GeneralSecurityException;
    void showHistory(String title) throws RemoteException, GeneralSecurityException;
    void run() throws RemoteException;
    PublicKey getKey() throws RemoteException;

}
