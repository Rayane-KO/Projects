// rkouidan
package com.vub.pdproject;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;

public class MainClient {
    public static void main(String[] args) throws IOException, NoSuchAlgorithmException {
        SystemSetup systemSetup = new SystemSetup();
        systemSetup.setupClient();
    }
}
