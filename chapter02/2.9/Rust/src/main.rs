#[derive(Debug)]
enum AppError {
    ParseError(#[allow(dead_code)] char),
    IoError(#[allow(dead_code)] std::io::Error),
}

fn digit_of_char(c: char) -> Result<u8, AppError> {
    match c {
        '0'..='9' => Ok((c as u8) - b'0'),
        'A'..='Z' => Ok((c as u8) - b'A' + 10),
        _ => Err(AppError::ParseError(c)),
    }
}

fn run_loop() -> Result<(), AppError> {
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    let mut buf = String::with_capacity(1024);

    loop {
        buf.clear();

        match std::io::BufRead::read_line(&mut handle, &mut buf).map_err(AppError::IoError)? {
            0 => break, // EOF
            _ => {
                let input = buf.trim();
                println!("{}", input);

                let result = input
                    .chars()
                    .map(digit_of_char)
                    .collect::<Result<Vec<u8>, AppError>>()?
                    .iter()
                    .fold(0, |acc, &x| (x as usize) + acc * 16);

                println!(" -> {:}\n", result)
            }
        }
    }
    Ok(())
}

fn main() {
    match run_loop() {
        Err(x) => eprintln!("Program failed: {:?}", x),
        Ok(()) => {}
    }
}
