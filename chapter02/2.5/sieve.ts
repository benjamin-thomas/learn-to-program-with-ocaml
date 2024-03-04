import * as readline from 'readline';

/*
RL=1 bun ./sieve.ts
bun --watch ./sieve.ts
*/

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function get_max(): Promise<number> {
    return new Promise((resolve) => {
        rl.question("Enter a number: ", (answer) => {
            resolve(parseInt(answer));
        });
    });
}

function compute(max: number, prime: boolean[]): void {
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

async function main(): Promise<void> {
    const max =
        process.env.RL === '1'
            ? await get_max()
            : 25;

    const prime: boolean[] = new Array(max + 1).fill(true);

    compute(max, prime);
    for (let n = 2; n <= max; n++) {
        if (prime[n]) {
            console.log(n);
        }
    }

    rl.close();
}

main()
    .then(() => console.log("DONE"))
    .catch(() => console.log("OOPS"))
    ;
