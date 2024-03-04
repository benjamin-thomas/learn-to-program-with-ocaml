using System;

class SieveOfEratosthenes
{
    static int GetMax()
    {
        Console.Write("Enter a number: ");
        return int.Parse(Console.ReadLine());
    }

    static void Compute(int max, bool[] prime)
    {
        int limit = (int)Math.Sqrt(max);
        prime[0] = false;
        prime[1] = false;
        for (int n = 2; n <= limit; n++)
        {
            if (prime[n])
            {
                int m = n * n;
                while (m <= max)
                {
                    prime[m] = false;
                    m += n;
                }
            }
        }
    }

    static void Main()
    {
        int max = GetMax();
        bool[] prime = new bool[max + 1];
        for (int i = 0; i <= max; i++)
        {
            prime[i] = true;
        }

        Compute(max, prime);

        for (int n = 2; n <= max; n++)
        {
            if (prime[n])
            {
                Console.WriteLine(n);
            }
        }
    }
}
