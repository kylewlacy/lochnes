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

pub struct NullVideo;
impl Video for NullVideo {
    fn draw_point(&mut self, _point: Point, _color: Color) { }
    fn present(&mut self) { }
    fn clear(&mut self) { }
}
