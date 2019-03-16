use sdl2::render::{Canvas, RenderTarget};
use sdl2::pixels::Color as SdlColor;
use sdl2::rect::Point as SdlPoint;

pub struct Point {
    pub x: u16,
    pub y: u16,
}

pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub trait Video {
    fn draw_point(&mut self, point: Point, color: Color);
    fn present(&mut self);
    fn clear(&mut self);
}

#[cfg(test)]
pub struct NullVideo;

#[cfg(test)]
impl Video for NullVideo {
    fn draw_point(&mut self, _point: Point, _color: Color) { }
    fn present(&mut self) { }
    fn clear(&mut self) { }
}

pub struct CanvasVideo<T: RenderTarget>(pub Canvas<T>);
impl<T: RenderTarget> Video for CanvasVideo<T> {
    fn draw_point(&mut self, point: Point, color: Color) {
        self.0.set_draw_color(SdlColor::RGB(color.r, color.g, color.b));
        self.0.draw_point(SdlPoint::new(point.x as i32, point.y as i32))
            .unwrap();
    }

    fn present(&mut self) {
        self.0.present();
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}
