import java.util.Scanner;

/*
 RL=1 java ./Sieve.java

 echo ./Sieve.java | entr -c java /_
 */

public class Sieve {
    public static int getMax() {
        try (Scanner scanner = new Scanner(System.in)) {
            System.out.print("Enter a number: ");
            return scanner.nextInt();
        }
    }

    public static void compute(int max, boolean[] prime) {
        int limit = (int) Math.sqrt(max);
        prime[0] = false;
        prime[1] = false;

        for (int n = 2; n <= limit; n++) {
            if (prime[n]) {
                int m = n * n;
                while (m <= max) {
                    prime[m] = false;
                    m += n;
                }
            }
        }
    }

    public static void main(String[] args) {
        int max = "1".equals(System.getenv("RL"))
                ? getMax()
                : 25;

        boolean[] prime = new boolean[max + 1];
        for (int i = 0; i <= max; i++) {
            prime[i] = true;
        }
        compute(max, prime);
        for (int n = 2; n <= max; n++) {
            if (prime[n]) {
                System.out.println(n);
            }
        }
    }
}
