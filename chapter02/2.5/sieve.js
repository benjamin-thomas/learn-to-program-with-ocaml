
/*
node ./sieve.js
*/

const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function get_max() {
    return new Promise((resolve, _reject) => {
        rl.question("Enter a number: ", (answer) => {
            resolve(parseInt(answer));
        });
    });
}

function compute(max, prime) {
    const limit = Math.trunc(Math.sqrt(max));
    prime[0] = false;
    prime[1] = false;
    for (let n = 2; n <= limit; n++) {
        if (prime[n]) {
            let m = n * n;
            while (m <= max) {
                prime[m] = false;
                m += n;
            }
        }
    }
}

(async () => {
    const max = await get_max();
    const prime = Array(max + 1).fill(true);
    compute(max, prime);
    for (let n = 2; n <= max; n++) {
        if (prime[n]) {
            console.log(n);
        }
    }
    rl.close();
})();
