package main

import (
	"fmt"
	"math"
	"os"
)

/*

Keeping in a separate directory, as the presence of a C file generates cascading errors in vscode otherwise.

RL=1 go run ./sieve.go
echo ./sieve.go | entr -c go run /_
*/

func getMax() int {
	var max int
	fmt.Print("Enter a number: ")
	fmt.Scan(&max)
	return max
}

func compute(max int, prime []bool) {
	limit := int(math.Sqrt(float64(max)))
	prime[0] = false
	prime[1] = false
	for n := 2; n <= limit; n++ {
		if prime[n] {
			for m := n * n; m <= max; m += n {
				prime[m] = false
			}
		}
	}
}

func main() {
	var max int
	if os.Getenv("RL") == "1" {
		max = getMax()
	} else {
		max = 25
	}
	prime := make([]bool, max+1)
	for i := range prime {
		prime[i] = true
	}

	compute(max, prime)

	for n := 2; n <= max; n++ {
		if prime[n] {
			fmt.Println(n)
		}
	}
}
