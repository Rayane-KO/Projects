// rkouidan
package com.vub.pdproject;

import com.vub.pdproject.protocols.ClientProtocol;
import com.vub.pdproject.protocols.DataPeerProtocol;
import com.vub.pdproject.protocols.UserDatabaseProtocol;
import com.vub.pdproject.protocols.WikipediaServerProtocol;

import java.io.IOException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.List;

public class SystemSetup {
    /**
     * Set up the connection to the RMI registry
     * @param server The remote server
     * @param name The name under which the server is bounded
     * @param <T> The type of the remote object
     */
    private <T extends Remote> void setupRMI(T server, String name) throws RemoteException {
        try {
            T stub = (T) UnicastRemoteObject.exportObject(server, 0);
            LocateRegistry.getRegistry().rebind(name, stub);
            System.out.println(name + " bound in registry");
        } catch (RemoteException e) {
            System.err.println(e.getMessage());
        }
    }
    public WikipediaServer setupSystem(List<Integer> dataPeersCapacities) throws IOException {
        try {
            LocateRegistry.createRegistry(1099);

            UserDatabaseServer userDatabase = new UserDatabaseServer();
            setupRMI(userDatabase, "UserDatabaseServer");

            Registry registry = LocateRegistry.getRegistry("localhost");
            WikipediaServer wikipediaServer = new WikipediaServer(dataPeersCapacities);
            setupRMI(wikipediaServer, "WikipediaServer");
            UserDatabaseProtocol userDatabaseStub = (UserDatabaseProtocol) registry.lookup("UserDatabaseServer");
            wikipediaServer.setUserDatabase(userDatabaseStub);

            int peerID = 0;
            for(int peerCapacity: dataPeersCapacities) {
                DataPeer dataPeer = new DataPeer(peerCapacity);
                String peerName = "DataPeerServer" + peerID;
                setupRMI(dataPeer, peerName);
                DataPeerProtocol dataPeerStub = (DataPeerProtocol) registry.lookup(peerName);
                wikipediaServer.addPeer(dataPeerStub);
            }
            System.out.println("All servers are running!");
            return wikipediaServer;
        } catch (NotBoundException | GeneralSecurityException e) {
            System.err.println("Could not setup the system correctly. Try again later!");
            return null;
        }
    }
    public void setupClient() {
        try {
            Registry registry = LocateRegistry.getRegistry("localhost");

            Client client = new Client();
            ClientProtocol stub = (ClientProtocol) UnicastRemoteObject.exportObject(client, 0);
            WikipediaServerProtocol wikipediaServerStub = (WikipediaServerProtocol) registry.lookup("WikipediaServer");
            client.setWikipediaServer(wikipediaServerStub);
            System.out.println("** WELCOME TO THE WIKIPEDIA SYSTEM! ** \n");
            client.run();
        } catch (NotBoundException | RemoteException | GeneralSecurityException e) {
            System.err.println("Could not setup the client correctly. Try again later!");
        }
    }
}
