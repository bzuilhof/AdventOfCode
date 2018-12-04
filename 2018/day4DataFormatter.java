package com.company;

import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class Main {

    public static void main(String[] args) throws FileNotFoundException {

        Solution sol = new Solution();
        String[] input = sol.getInput();
        List<Entry> entries = new ArrayList<>();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        for (String s : input) {
            String dt = s.substring(1, 17);
            LocalDateTime dateTime = LocalDateTime.parse(dt, formatter);
            String type = s.substring(19, 20);
            Type typeE = sol.getType(type);
            Entry entry = new Entry(typeE, dateTime);
            if (typeE ==  Type.START) {
                String gn = s.substring(26,29).trim();
                entry.guardNumber = Integer.parseInt(gn);

            }
            entries.add(entry);
        }
        Collections.sort(entries);


        HashMap<Integer, String> sleepTimer = new HashMap<>();
        int currentGuard = 0;

        for (Entry entry : entries) {
            if (entry.type == Type.START) currentGuard = entry.guardNumber;
            else {
                String sleepS = sleepTimer.containsKey(currentGuard) ? sleepTimer.get(currentGuard) : Integer.toString(currentGuard);

                sleepS += " ";
                sleepS += Integer.toString(entry.stamp.getMinute());
                sleepTimer.put(currentGuard, sleepS);
            }
        }

        System.out.println();

        PrintWriter out = new PrintWriter("formatted.txt");
        for (String s : sleepTimer.values()) {
            out.println(s);
        }
        out.close();
    }
}

class Solution {
   String[] getInput() {
       try (BufferedReader br = new BufferedReader(new FileReader("src/input.txt"))) {
           StringBuilder sb = new StringBuilder();
           String line = br.readLine();

           while (line != null) {
               sb.append(line);
               sb.append(System.lineSeparator());
               line = br.readLine();
           }
           String everything = sb.toString();

           return everything.split("\\n");
       } catch (IOException e) {
           e.printStackTrace();
       }
        return new String[]{};
   }

   Type getType(String s) {
       if (s.equals("G")) return Type.START;
       if (s.equals("w")) return Type.WAKE;
       if (s.equals("f")) return Type.SLEEP;
       return Type.ERROR;
   }
}

class Entry implements Comparable<Entry> {
    Type type;
    LocalDateTime stamp;
    int guardNumber;

    Entry(Type type, LocalDateTime stamp) {
        this.type = type;
        this.stamp = stamp;
    }

    @Override
    public int compareTo(Entry o) {
        return stamp.compareTo(o.stamp);
    }
}

enum Type {
    START,
    SLEEP,
    WAKE,
    ERROR

}