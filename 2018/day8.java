package com.company;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Main {
    public static void main(String[] args) throws IOException {
        Answer answer = new Answer();
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String[] x = br.readLine().split(" ");
        int[] inputArr = Arrays.stream(x).mapToInt(Integer::parseInt).toArray();
        System.out.println(answer.findSum(inputArr));
    }
}

class Answer {
    private int[] input;
    private int p = 0;
    int findSum(int[] input) {
        int sum = 0;
        this.input = input;
        sum = recNodeP2();


        return sum;
    }

    private int recNodeP1() {
        int sum = 0;
        int childNodes = input[p];
        p++;

        int entries = input[p];
        p++;

        for (int i = 0; i < childNodes; i++) {
            sum += recNodeP1();
        }
        for (int i = 0; i < entries; i++) {
            sum += input[p];
            p++;
        }

        return sum;

    }
    private int recNodeP2() {
        int sum = 0;
        int[] childNodes = new int[input[p]];
        p++;

        int entries = input[p];
        p++;

        for (int i = 0; i < childNodes.length; i++) {
            childNodes[i] = recNodeP2();
        }
        for (int i = 0; i < entries; i++) {
            if (childNodes.length == 0) {
                sum += input[p];
            } else {
                if(input[p] <= childNodes.length) sum += childNodes[input[p]-1];
            }
            p++;
        }

        return sum;
    }
}

