// rkouidan
package com.vub.pdproject;

import com.vub.pdproject.data.WikipediaData.WikipediaDataSet;
import com.vub.pdproject.search.QueryEngine;
import com.vub.pdproject.search.QueryEngine.RRecord;
import com.vub.pdproject.search.SequentialSearch;
import com.vub.pdproject.search.WikipediaQuery;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.List;

public class MainServer {
    public static void main(String[] args) throws IOException, NoSuchAlgorithmException {
        SystemSetup systemSetup = new SystemSetup();
        // 3 data peers with capacity of 10
        List<Integer> dataPeersCapacities = Arrays.asList(10, 10, 10);
        systemSetup.setupSystem(dataPeersCapacities);
    }
}