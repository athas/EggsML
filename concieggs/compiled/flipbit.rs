use std::{env, fs::{self, File}, io::Read as _};

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut args = env::args();
    let _ = args.next(); // Exe navn
    let filename = args.next().ok_or("No file given")?;
    let mut file = fs::read(&filename)?;
    let flip1 = get_rand()? % file.len();
    let flip2 = get_rand()? % u8::BITS as usize;
    let flipper = 1u8 << flip2;
    file[flip1] ^= flipper;
    fs::write(&filename, file)?;
    Ok(())
}

fn get_rand() -> Result<usize, Box<dyn std::error::Error + Send + Sync>> {
    let mut f = File::open("/dev/urandom")?;
    let mut buf = [0u8; ((usize::BITS / 8) as usize)];
    f.read_exact(&mut buf)?;
    let n = usize::from_ne_bytes(buf);
    Ok(n)
}

