use anyhow::Result;
use image::{ImageReader, DynamicImage};

fn main() -> Result<()> {
    let img = ImageReader::open("../hamlet_encoded.bmp")?.decode()?;

    let DynamicImage::ImageRgb8(image) = img else {
        anyhow::bail!("Image in unrecognized format :(");
    };

    let s = image
        .into_raw()
        .chunks(8)
        .map(|bytes: &[u8]| -> char {
            let code = bytes.iter().fold(0, |acc, byte| (acc << 1) | (byte & 1));
            if code == 25 {
                '\u{2019}'
            } else {
                code as char
            }
        })
        .take_while(|&ch| ch != '\0')
        .collect::<String>();

    println!("{s}");
    Ok(())
}
