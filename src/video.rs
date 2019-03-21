use sdl2::render::{Canvas, RenderTarget};
use sdl2::pixels::Color as SdlColor;
use sdl2::rect::Point as SdlPoint;

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

pub struct ScaleVideo<V: Video> {
    video: V,
    scale: u16,
}

impl<V: Video> ScaleVideo<V> {
    pub fn new(video: V, scale: u16) -> Self {
        ScaleVideo { video, scale }
    }
}

impl<V: Video> Video for ScaleVideo<V> {
    fn draw_point(&mut self, point: Point, color: Color) {
        for scale_x in 0..self.scale {
            for scale_y in 0..self.scale {
                let scale_point = Point {
                    x: (point.x * self.scale) + scale_x,
                    y: (point.y * self.scale) + scale_y,
                };
                self.video.draw_point(scale_point, color);
            }
        }
    }

    fn present(&mut self) {
        self.video.present();
    }

    fn clear(&mut self) {
        self.video.clear();
    }
}
