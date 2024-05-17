#![allow(unused_imports)]
#![allow(unused_variables)]

use windows::core::*;
use windows::Win32::Foundation::*;
use windows::Win32::Graphics::Dwm::*;
use windows::Win32::Graphics::Gdi::*;
use windows::Win32::System::LibraryLoader::*;
use windows::Win32::UI::Input::KeyboardAndMouse::*;
use windows::Win32::UI::WindowsAndMessaging::*;

use std::mem;
use std::ptr;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::atomic::*;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;
use std::time::Instant;

static SHOULD_RUN: AtomicBool = AtomicBool::new(true);

static FRAMEBUFFERS: [FrameBuffer; 2] =
  [FrameBuffer::new(), FrameBuffer::new()];
struct FrameBuffer {
  pub pixels: Mutex<Vec<COLORREF>>,
  pub window_size: AtomicWindowSize,
}
impl FrameBuffer {
  pub const fn new() -> Self {
    Self {
      pixels: Mutex::new(Vec::new()),
      window_size: AtomicWindowSize::new(),
    }
  }

  pub fn buffer_len(&self) -> usize {
    let (width, height) = self.window_size.load();
    width * height
  }

  pub fn resize(&self, width: usize, height: usize) {
    let (self_width, self_height) = self.window_size.load();
    if width * height > self_width * self_height {
      self.pixels.lock().unwrap().resize(width * height, COLORREF(0));
    }
    self.window_size.store(width as _, height as _);
  }
}

static FRONT_BUFFER_INDEX: AtomicUsize = AtomicUsize::new(0);
static SWAP_BUFFERS: AtomicBool = AtomicBool::new(true);
fn front_buffer_index() -> usize {
  FRONT_BUFFER_INDEX.load(SeqCst)
}
fn back_buffer_index() -> usize {
  FRONT_BUFFER_INDEX.load(SeqCst) ^ 1
}
fn perform_buffer_swap() {
  let should_swap = SWAP_BUFFERS.load(SeqCst);
  if should_swap {
    let _ = FRONT_BUFFER_INDEX.fetch_xor(1, SeqCst);
    SWAP_BUFFERS.store(false, SeqCst);
  }
}

static TARGET_WINDOW_SIZE: AtomicWindowSize = AtomicWindowSize::new();
struct AtomicWindowSize {
  inner: AtomicUsize,
}
impl AtomicWindowSize {
  pub const fn new() -> Self {
    Self { inner: AtomicUsize::new(0) }
  }

  pub fn load(&self) -> (usize, usize) {
    let inner = self.inner.load(SeqCst);
    let width = (inner >> 32) & 0xFFFF_FFFF;
    let height = inner & 0xFFFF_FFFF;
    (width, height)
  }

  pub fn store(&self, width: usize, height: usize) {
    assert!(width <= 0x7FFF_FFFF && height <= 0x7FFF_FFFF);
    let inner = (width << 32) | height;
    self.inner.store(inner, SeqCst);
  }
}

pub fn rgb_to_colorref(r: u8, g: u8, b: u8) -> COLORREF {
  let r = (r as u32) << 16;
  let g = (g as u32) << 8;
  let b = b as u32;
  let result = b | g | r;
  COLORREF(result)
}
pub fn colorref_to_rgb(color: COLORREF) -> (u8, u8, u8) {
  let c = color.0;
  let r = ((c >> 16) & 0xFF) as u8;
  let g = ((c >> 8) & 0xFF) as u8;
  let b = (c & 0xFF) as u8;
  (r, g, b)
}

static GAMESTATE: GameState = GameState::new();
struct GameState {
  pub rgb: AtomicUsize,
}
impl GameState {
  pub const fn new() -> Self {
    Self { rgb: AtomicUsize::new(0) }
  }
}

fn update() {
  let rgb = GAMESTATE.rgb.load(SeqCst);
  let r = ((rgb & 0xFF_0000) + (2 << 16)) & 0xFF_0000;
  let g = ((rgb & 0x00_FF00) + (3 << 8)) & 0x00_FF00;
  let b = ((rgb & 0x00_00FF) + 5) & 0xFF;
  let new_rgb = r | g | b;
  GAMESTATE.rgb.compare_exchange(rgb, new_rgb, SeqCst, SeqCst).unwrap();
}

fn render() {
  let rgb = GAMESTATE.rgb.load(SeqCst);
  let r = (rgb & 0x00FF_0000) >> 16;
  let g = (rgb & 0x0000_FF00) >> 8;
  let b = rgb & 0x0000_00FF;

  let back_buffer = &FRAMEBUFFERS[back_buffer_index()];

  let (win_width, win_height) = TARGET_WINDOW_SIZE.load();
  back_buffer.resize(win_width, win_height);

  let mut pixels = back_buffer.pixels.try_lock().unwrap();
  let color = rgb_to_colorref(r as _, g as _, b as _);
  for i in 0..back_buffer.buffer_len() {
    pixels[i] = color;
  }
}

fn main() -> Result<()> {
  unsafe {
    let instance_handle = GetModuleHandleA(PCSTR::null())?;
    assert!(instance_handle.0 != 0);
    let window_class_name = s!("WindowClassName");
    let window_name = s!("ZInvaders");

    let window_class = WNDCLASSA {
      style: CS_HREDRAW | CS_VREDRAW, // & WNDCLASS_STYLES(0),
      lpfnWndProc: Some(window_proc),
      hInstance: instance_handle.into(),
      lpszClassName: window_class_name,
      ..Default::default()
    };

    let atom = RegisterClassA(&window_class);
    assert!(atom != 0);

    let window_handle = CreateWindowExA(
      WINDOW_EX_STYLE::default(),
      window_class_name,
      window_name,
      WS_OVERLAPPEDWINDOW, // | WS_DISABLED,    // window style
      CW_USEDEFAULT,       // x
      CW_USEDEFAULT,       // y
      CW_USEDEFAULT,       // nWidth
      CW_USEDEFAULT,       // nHeight
      None,                // parent window handle
      None,                // menu handle
      instance_handle,     // instance
      None,                // lpParam
    );
    assert!(window_handle.0 != 0);

    let hdc = GetDC(window_handle);
    assert!(hdc.0 != 0);

    let mut client_rect = RECT::default();
    GetClientRect(window_handle, &mut client_rect).unwrap();
    TARGET_WINDOW_SIZE.store(
      (client_rect.right - client_rect.left) as _,
      (client_rect.bottom - client_rect.top) as _,
    );

    // let _ = EnableWindow(window_handle, true);
    let _ = ShowWindow(window_handle, SW_SHOW);
    // assert!(ValidateRect(window_handle, None).0 != 0);

    let mut last_instant = Instant::now();
    let mut message = MSG::default();

    while SHOULD_RUN.load(Ordering::SeqCst) {
      while PeekMessageA(&mut message, None, 0, 0, PM_REMOVE).into() {
        DispatchMessageA(&message);
      }

      update(); // TODO: make this use dt

      let ready = SWAP_BUFFERS.load(SeqCst);
      if !ready {
        render();

        SWAP_BUFFERS.store(true, SeqCst);
      }

      draw(hdc);
      DwmFlush().unwrap(); // wait for Vsync

      // thread::sleep(Duration::from_millis(1));
      let now = Instant::now();
      let frame_time = now.duration_since(last_instant);
      println!(
        "Frame {} ms, front: {}, back: {}",
        frame_time.as_secs_f32() * 1000.0,
        front_buffer_index(),
        back_buffer_index(),
      );
      last_instant = now;
    }

    PostQuitMessage(0);
    Ok(())
  }
}

unsafe fn draw(hdc: HDC) {
  perform_buffer_swap();

  let buffer = &FRAMEBUFFERS[front_buffer_index()];
  let (img_width, img_height) = buffer.window_size.load();
  let (win_width, win_height) = TARGET_WINDOW_SIZE.load();

  if img_width <= 0 || img_height <= 0 {
    return;
  }
  if win_width <= 0 || win_height <= 0 {
    return;
  }

  let img_width = img_width.try_into().unwrap();
  let img_height = img_height.try_into().unwrap();
  let win_width = win_width.try_into().unwrap();
  let win_height = win_height.try_into().unwrap();

  let mut header = BITMAPINFO::default();
  header.bmiHeader.biSize = mem::size_of_val(&header) as _;
  header.bmiHeader.biWidth = img_width;
  header.bmiHeader.biHeight = img_height;
  header.bmiHeader.biPlanes = 1; // must be 1
  header.bmiHeader.biBitCount = 32;
  header.bmiHeader.biCompression = BI_RGB.0;

  let pixels = buffer.pixels.try_lock().unwrap();
  // dbg!(&pixels);

  let result = StretchDIBits(
    hdc,
    0,          // xDest
    0,          // yDest
    win_width,  // DestWidth
    win_height, // DestHeight
    0,          // xSrc
    0,          // ySrc
    img_width,  // SrcWidth
    img_height, // SrcHeight
    Some(pixels.as_ptr() as _),
    &header,
    DIB_RGB_COLORS,
    SRCCOPY,
  );
  assert!(result != 0);
}

extern "system" fn window_proc(
  window: HWND,
  message: u32,
  wparam: WPARAM,
  lparam: LPARAM,
) -> LRESULT {
  unsafe {
    if message == WM_PAINT {
      let mut paint = PAINTSTRUCT::default();
      BeginPaint(window, &mut paint);
      draw(paint.hdc);
      let _ = EndPaint(window, &paint);
    } else if message == WM_CLOSE || message == WM_DESTROY {
      SHOULD_RUN.store(false, Ordering::SeqCst);
    } else if message == WM_ERASEBKGND {
      return LRESULT(1); // tell Windows we processed this
    } else if message == WM_QUIT {
      panic!("Got WM_QUIT. Windows should never send this message");
    } else if message == WM_SIZE {
      let width = lparam.0 & 0xFFFF;
      let height = (lparam.0 >> 16) & 0xFFFF;
      TARGET_WINDOW_SIZE.store(width as _, height as _);
    } else {
      return DefWindowProcA(window, message, wparam, lparam);
    }
    LRESULT(0)
  }
}
