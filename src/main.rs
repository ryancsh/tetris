#![allow(unused_imports)]
#![allow(unused_variables)]

use windows::core::*;
use windows::Win32::Foundation::*;
use windows::Win32::Graphics::Dwm::*;
use windows::Win32::Graphics::Gdi::*;
use windows::Win32::UI::WindowsAndMessaging::*;
use windows::Win32::System::LibraryLoader::*;
use windows::Win32::UI::Input::KeyboardAndMouse::*;

use std::ptr;
use std::mem;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use std::sync::atomic::*;
use std::sync::atomic::Ordering::SeqCst;

static SHOULD_RUN : AtomicBool = AtomicBool::new(true);

static FRONT_BUFFER: AtomicPtr<FrameBuffer> = AtomicPtr::new(ptr::null_mut());
static BACK_BUFFER: AtomicPtr<FrameBuffer> = AtomicPtr::new(ptr::null_mut());
static QUEUED_BUFFER: AtomicPtr<FrameBuffer> = AtomicPtr::new(ptr::null_mut());

static TARGET_WINDOW_SIZE: AtomicWindowSize = AtomicWindowSize::new();

struct AtomicWindowSize {
  inner: AtomicIsize
}
impl AtomicWindowSize {
  pub const fn new() -> Self {
    Self {inner: AtomicIsize::new(0) }
  }

  pub fn load(&self) -> (isize, isize) {
    let inner = self.inner.load(SeqCst);
    let width = (inner >> 16) & 0xFFFF;
    let height = inner & 0xFFFF;
    (width, height)
  }

  pub fn store(&self, width: isize, height: isize) {
    let width = width.max(0);
    let height = height.max(0);
    let inner = (width << 16) | height;
    self.inner.store(inner, SeqCst);
  }
}

struct FrameBuffer {
  pub pixels: Vec<COLORREF>,
  pub width: usize,
  pub height: usize,
}
impl FrameBuffer {
  pub fn new_mut() -> *mut Self {
    let mut buffer = Self {
      pixels: Vec::new(),
      width: 0,
      height: 0,
    };
    Box::into_raw(Box::new(buffer))
  }

  pub fn resize(&mut self, width: usize, height: usize) {
    if width * height > self.width * self.height {
      self.pixels.resize(width * height, COLORREF(0));
    }
    self.width = width;
    self.height = height;
  }
}

pub fn rgb(r: u8, g: u8, b: u8) -> COLORREF {
  let r = (r as u32) << 16;
  let g = (g as u32) << 8;
  let b = b as u32;
  let result = b | g | r;
  COLORREF(result)
}
pub fn get_rgb(color: COLORREF) -> (u8, u8, u8) {
  let c = color.0;
  let r = ((c >> 16) & 0xFF) as u8;
  let g = ((c >> 8) & 0xFF) as u8;
  let b = (c & 0xFF) as u8;
  (r, g, b)
}

fn main() -> Result<()> {
  unsafe {
    let instance_handle = GetModuleHandleA(PCSTR::null())?;
    assert!(instance_handle.0 != 0);
    let window_class_name = s!("WindowClassName");
    let window_name = s!("Tetris");

    let mut front_buffer = FrameBuffer::new_mut();
    (&mut *front_buffer).resize(2,2);
    (*front_buffer).pixels[0] = rgb(0xFF, 0x00, 0x00);
    (*front_buffer).pixels[1] = rgb(0x00, 0xFF, 0x00);
    (*front_buffer).pixels[2] = rgb(0x00, 0x00, 0xFF);
    (*front_buffer).pixels[3] = rgb(0xFF, 0xFF, 0xFF);
    FRONT_BUFFER.store(front_buffer, SeqCst);

    let mut queued_buffer = FrameBuffer::new_mut();
    (&mut *queued_buffer).resize(2,2);
    (*queued_buffer).pixels[0] = rgb(0x00, 0xFF, 0xFF);
    (*queued_buffer).pixels[1] = rgb(0xFF, 0xFF, 0x00);
    (*queued_buffer).pixels[2] = rgb(0xFF, 0x00, 0xFF);
    (*queued_buffer).pixels[3] = rgb(0xFF, 0xFF, 0xFF);
    QUEUED_BUFFER.store(queued_buffer, SeqCst);

    let window_class = WNDCLASSA {
      style: CS_HREDRAW | CS_VREDRAW,
      lpfnWndProc: Some(window_proc),
      hInstance: instance_handle.into(),
      lpszClassName: window_class_name,
      .. Default::default()
    };

    let atom = RegisterClassA(&window_class);
    assert!(atom != 0);

    let window_handle = CreateWindowExA(
      WINDOW_EX_STYLE::default(),
      window_class_name,
      window_name,
      WS_OVERLAPPEDWINDOW, // | WS_DISABLED,    // window style
      CW_USEDEFAULT,        // x
      CW_USEDEFAULT,        // y
      CW_USEDEFAULT,        // nWidth
      CW_USEDEFAULT,        // nHeight
      None,                 // parent window handle
      None,                 // menu handle
      instance_handle,      // instance
      None,                 // lpParam
      );
    let error = GetLastError();
    assert!(window_handle.0 != 0);

    let hdc = GetDC(window_handle);
    assert!(hdc.0 != 0);

    let mut client_rect = RECT::default();
    GetClientRect(window_handle, &mut client_rect).unwrap();
    TARGET_WINDOW_SIZE.store(
      (client_rect.right - client_rect.left) as isize,
      (client_rect.bottom - client_rect.top) as isize);

    // let _ = EnableWindow(window_handle, true);
    let _ = ShowWindow(window_handle, SW_SHOW);
    // assert!(ValidateRect(window_handle, None).0 != 0);

    let mut last_instant = Instant::now();

    let mut message = MSG::default();
    while SHOULD_RUN.load(Ordering::SeqCst) {
      while PeekMessageA(&mut message, None, 0, 0, PM_REMOVE).into() {
        DispatchMessageA(&message);
      }

      // update state

      let back_buffer = BACK_BUFFER.swap(ptr::null_mut(), SeqCst);
      if !back_buffer.is_null() {
        // resize buffer if needed based on target window size
        // render into it
        let queued = QUEUED_BUFFER.swap(back_buffer, SeqCst);
        assert!(queued.is_null());
      }

      draw(hdc);
      // DwmFlush().unwrap();  // wait for Vsync

      // thread::sleep(Duration::from_millis(1));
      let now = Instant::now();
      let frame_time = now.duration_since(last_instant);
      if frame_time > Duration::from_millis(10) {
        println!("Slow frame {} ms", frame_time.as_secs_f32() * 1000.0);
      }
      last_instant = now;
    }
    
    PostQuitMessage(0);
    Ok(())
  }
}

unsafe fn draw(hdc: HDC) {
  let queued = QUEUED_BUFFER.swap(ptr::null_mut(), SeqCst);
  if !queued.is_null() {
    let front = FRONT_BUFFER.swap(queued, SeqCst);
    assert!(!front.is_null());
    let back = BACK_BUFFER.swap(front, SeqCst);
    assert!(back.is_null());
  }

  let buffer = FRONT_BUFFER.load(SeqCst);
  assert!(!buffer.is_null());
  let buffer = &*buffer;

  let image_width = buffer.width.try_into().unwrap();
  let image_height = buffer.height.try_into().unwrap();

  let (window_width, window_height) = TARGET_WINDOW_SIZE.load();
  let window_width = window_width.try_into().unwrap();
  let window_height = window_height.try_into().unwrap();

  if window_width == 0 || window_height == 0 || image_width == 0 || image_height == 0 {
    return;
  }

  let mut header = BITMAPINFO::default();
  header.bmiHeader.biSize = mem::size_of_val(&header) as _;
  header.bmiHeader.biWidth = image_width;
  header.bmiHeader.biHeight = image_height;
  header.bmiHeader.biPlanes = 1;  // must be 1
  header.bmiHeader.biBitCount = 32;
  header.bmiHeader.biCompression = BI_RGB.0;

  let result = StretchDIBits(hdc, 0, 0, window_width, window_height, 0, 0, image_width, image_height, Some(buffer.pixels.as_ptr() as _), &header, DIB_RGB_COLORS, SRCCOPY);
  assert!(result != 0);
}

extern "system" fn window_proc(window: HWND, message: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
  unsafe {
    if message == WM_PAINT {
      let mut paint = PAINTSTRUCT::default();
      BeginPaint(window, &mut paint);
      draw(paint.hdc);
      let _ = EndPaint(window, &paint);
    } else if message == WM_CLOSE || message == WM_DESTROY {
      SHOULD_RUN.store(false, Ordering::SeqCst);
    } else if message == WM_ERASEBKGND {
      return LRESULT(1);    // tell Windows we processed this
    } else if message == WM_QUIT {
      panic!("Got WM_QUIT. Windows should never send this message");
    } else if message == WM_SIZE {
      let width = lparam.0 & 0xFFFF;
      let height = (lparam.0 >> 16) & 0xFFFF;
      TARGET_WINDOW_SIZE.store(width, height);
    } else {
      return DefWindowProcA(window, message, wparam, lparam);
    }
    LRESULT(0)
  }
}
