use std::sync::RwLock;
use sdl2::render::{Canvas, RenderTarget, Texture, TextureCreator};
use sdl2::pixels::PixelFormatEnum::RGB24;

#[derive(Debug, Clone, Copy)]
pub struct Point {
    pub x: u16,
    pub y: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub trait Video {
    fn draw_point(&self, point: Point, color: Color);
    fn present(&self);
    fn clear(&self);
}

pub struct NullVideo;

impl Video for NullVideo {
    fn draw_point(&self, _point: Point, _color: Color) { }
    fn present(&self) { }
    fn clear(&self) { }
}

pub struct TextureBufferedVideo<'a> {
    buffer: RwLock<Vec<Color>>,
    width: u32,
    height: u32,
    frame: RwLock<Texture<'a>>,
}

impl<'a> TextureBufferedVideo<'a> {
    pub fn new<T>(
        texture_creator: &'a TextureCreator<T>,
        width: u32,
        height: u32
    )
        -> Result<Self, sdl2::render::TextureValueError>
    {
        let black = Color { r: 0, g: 0, b: 0 };
        let buffer = vec![black; width as usize * height as usize];
        let buffer = RwLock::new(buffer);
        let frame = texture_creator
            .create_texture_streaming(RGB24, width, height)?;
        let frame = RwLock::new(frame);

        Ok(TextureBufferedVideo {
            buffer,
            frame,
            width,
            height,
        })
    }

    pub fn copy_to(&self, canvas: &mut Canvas<impl RenderTarget>)
        -> Result<(), String>
    {
        let current_frame = self.frame.read().unwrap();
        canvas.copy(&current_frame, None, None)?;

        Ok(())
    }
}

impl<'a, 'b> Video for TextureBufferedVideo<'b> {
    fn draw_point(&self, point: Point, color: Color) {
        let mut buffer = self.buffer.write().unwrap();
        let offset = point.y as usize * self.width as usize + point.x as usize;
        buffer[offset] = color;
    }

    fn present(&self) {
        let buffer = self.buffer.read().unwrap();
        let mut frame = self.frame.write().unwrap();
        frame.with_lock(None, |frame_buffer, pitch| {
            for y in 0..self.height {
                for x in 0..self.width {
                    let offset = y as usize * self.width as usize + x as usize;
                    let color = buffer[offset];

                    let frame_offset = y as usize * pitch + (x as usize * 3);
                    frame_buffer[frame_offset] = color.r;
                    frame_buffer[frame_offset + 1] = color.g;
                    frame_buffer[frame_offset + 2] = color.b;
                }
            }
        }).unwrap();
    }

    fn clear(&self) { }
}

impl<'a, V> Video for &'a V
    where V: Video
{
    fn draw_point(&self, point: Point, color: Color) {
        (*self).draw_point(point, color);
    }

    fn present(&self) {
        (*self).present();
    }

    fn clear(&self) {
        (*self).clear();
    }
}
