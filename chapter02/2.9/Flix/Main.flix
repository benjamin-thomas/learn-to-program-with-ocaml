/*

java -jar ~/.config/Code/User/globalStorage/flix.flix/flix.jar ./Main.flix

Or:

java -jar ~/.config/Code/User/globalStorage/flix.flix/flix.jar
flix> main() // and re-run on changes (it picks up the file changes!!)

java -jar ~/.config/Code/User/globalStorage/flix.flix/flix.jar ./Main.flix --args /usr/share/dict/words

 */

def wordsCount2(str: String): Int32 =
    List.length(String.words(str))

def wordsCount(str: String): Int32 =
    str
    |> String.words
    |> List.length

def run(file: String): Unit \ IO =
    println("Reading: ${file}");
    match Files.readLines(file) {
        case Err(_) => println("Could not read file: ${file}")
        case Ok(lines) =>
            let totalLines = List.length(lines);
            let totalWords = List.sumWith(wordsCount, lines);
            println("${totalLines} lines, ${totalWords} words")
    }


/*

flix> run2("/usr/share/dict/words"::Nil)
104334 lines, 104334 words

 */
def run2(args_: List[String]): Unit \ IO =
    let result =
        forM (
            file_ <- List.head(args_)
                        |> Option.toOk("Missing argument: filename");

            lines <- Files.readLines(file_)
                        |> Result.mapErr(_ -> "Unable to read: ${file_}")
        ) yield {
            let totalLines = List.length(lines);
            let totalWords = List.sumWith(wordsCount, lines);
            (totalLines, totalWords)
        };
    match result {
        case Err(err) => println(err)
        case Ok((totalLines, totalWords)) =>
            println("${totalLines} lines, ${totalWords} words")
    }

def add(a: Int32, b: Int32): Int32 = a + b

/*

flix> :test

 */
@Test
def testAdd01(): Bool = 0 == add(0, 0)

// Use `Assert.eq` to print the difference between expected/actual values:t
@Test
def testAdd02(): Bool = Assert.eq(3, add(1, 2))

def concat(a: String, b: String): String = a + b

/*

In the REPL, we can just run the test function as-is:

flix> testAdd03()

 */
@Test
def testAdd03(): Bool = Assert.eq("HelloWorld", concat("Hello", "World"))


/*

The standard library documentation is available at:

https://api.flix.dev/

*/
def main(): Unit \ IO =
    let args = Environment.getArgs();
    match args  {
        case Nil      => println("Missing argument: filename")
        case file ::_ => run(file)
    }


