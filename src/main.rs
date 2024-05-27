#![allow(unused_imports)]
#![allow(unused_variables)]

use windows::core::*;

use windows::Win32::Foundation::*;
use windows::Win32::Graphics::Dwm::*;
use windows::Win32::Graphics::Gdi::*;
use windows::Win32::Graphics::OpenGL::*;
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

/*
mod draw;
use draw::*;
*/

static SHOULD_RUN: AtomicBool = AtomicBool::new(true);
static WINDOW_HANDLE: AtomicIsize = AtomicIsize::new(0);

static MONITOR_REFRESH_NANOS: AtomicU64 =
  AtomicU64::new(1_000_000_000 / 60);
unsafe fn update_monitor_refresh_rate() {
  let mut devmode = DEVMODEA::default();
  devmode.dmSize = mem::size_of_val(&devmode) as _;
  let result =
    EnumDisplaySettingsA(None, ENUM_CURRENT_SETTINGS, &mut devmode);
  let refresh_rate = devmode.dmDisplayFrequency;
  if result.0 != 0 && refresh_rate > 1 {
    let refresh_nanos = 1_000_000_000 / refresh_rate as u64;
    MONITOR_REFRESH_NANOS.store(refresh_nanos, SeqCst);
    RENDER_NANOS.store(0, SeqCst);
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Color {
  pub r: f32, // 0 to 1
  pub g: f32,
  pub b: f32,
  pub a: f32,
}
impl Color {
  pub const fn new() -> Self {
    Self { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
  }

  pub fn rgb(r: f32, g: f32, b: f32) -> Self {
    Color::rgba(r, g, b, 1.0)
  }

  pub fn rgba(r: f32, g: f32, b: f32, a: f32) -> Self {
    debug_assert!(r >= 0.0 && r <= 1.0);
    debug_assert!(g >= 0.0 && g <= 1.0);
    debug_assert!(b >= 0.0 && b <= 1.0);
    debug_assert!(a >= 0.0 && a <= 1.0);
    Color { r, g, b, a }
  }
}
impl Default for Color {
  fn default() -> Self {
    Self::new()
  }
}
impl From<COLORREF> for Color {
  fn from(c: COLORREF) -> Self {
    let r = (((c.0 >> 16) & 0xFF) as f32) / (u8::MAX as f32);
    let g = (((c.0 >> 8) & 0xFF) as f32) / (u8::MAX as f32);
    let b = ((c.0 & 0xFF) as f32) / (u8::MAX as f32);
    Self::rgb(r as _, g as _, b as _)
  }
}
impl From<Color> for COLORREF {
  fn from(c: Color) -> Self {
    let r = ((c.r * (u8::MAX as f32)) as u32) << 16;
    let g = ((c.g * (u8::MAX as f32)) as u32) << 8;
    let b = (c.b * (u8::MAX as f32)) as u32;
    let result = b | g | r;
    COLORREF(result)
  }
}

static BACKBUFFER: FrameBuffer<Color> = FrameBuffer::new();
static FRONTBUFFERS: [FrameBuffer<COLORREF>; 2] =
  [FrameBuffer::new(), FrameBuffer::new()];

struct FrameBuffer<T>
where
  T: Copy + Default,
{
  pub pixels: Mutex<Vec<T>>,
  pub size: Atomic2dSize,
}
impl<T> FrameBuffer<T>
where
  T: Copy + Default,
{
  pub const fn new() -> Self {
    Self { pixels: Mutex::new(Vec::new()), size: Atomic2dSize::new() }
  }

  pub fn resize(&self, width: usize, height: usize) {
    let (self_width, self_height) = self.size.load();
    if width * height > self_width * self_height {
      self.pixels.lock().unwrap().resize(width * height, T::default());
    }
    self.size.store(width as _, height as _);
  }
}

static FRONT_BUFFER_INDEX: AtomicUsize = AtomicUsize::new(0);
static SWAP_BUFFERS: AtomicBool = AtomicBool::new(false);
fn front_buffer_index() -> usize {
  FRONT_BUFFER_INDEX.load(SeqCst)
}
fn spare_buffer_index() -> usize {
  FRONT_BUFFER_INDEX.load(SeqCst) ^ 1
}
fn perform_buffer_swap() {
  let should_swap = SWAP_BUFFERS.load(SeqCst);
  if should_swap {
    let _ = FRONT_BUFFER_INDEX.fetch_xor(1, SeqCst);
    SWAP_BUFFERS.store(false, SeqCst);
  }
}

static RENDER_NANOS: AtomicU64 = AtomicU64::new(0);
static RENDER_SIZE: Atomic2dSize = Atomic2dSize::new();

static WINDOW_SIZE: Atomic2dSize = Atomic2dSize::new();
struct Atomic2dSize {
  inner: AtomicU64,
}
impl Atomic2dSize {
  pub const fn new() -> Self {
    Self { inner: AtomicU64::new(0) }
  }

  pub fn load(&self) -> (usize, usize) {
    let inner = self.inner.load(SeqCst);
    let width = (inner >> 32) & 0xFFFF_FFFF;
    let height = inner & 0xFFFF_FFFF;
    (width as usize, height as usize)
  }

  pub fn store(&self, width: usize, height: usize) {
    assert!(width <= 0x7FFF_FFFF && height <= 0x7FFF_FFFF);
    let inner = ((width as u64) << 32) | height as u64;
    self.inner.store(inner, SeqCst);
  }
}

static GAMESTATE: GameState = GameState::new();
#[derive(Debug)]
struct GameState {
  pub rgb: Mutex<Color>,
  pub x: AtomicUsize,

  pub action_up: AtomicBool,
  pub action_down: AtomicBool,
  pub action_left: AtomicBool,
  pub action_right: AtomicBool,
  pub action_space: AtomicBool,
}
impl GameState {
  pub const fn new() -> Self {
    Self {
      rgb: Mutex::new(Color::new()),
      x: AtomicUsize::new(0),
      action_up: AtomicBool::new(false),
      action_down: AtomicBool::new(false),
      action_left: AtomicBool::new(false),
      action_right: AtomicBool::new(false),
      action_space: AtomicBool::new(false),
    }
  }
}

/*
struct KeyState {
  pub ended_down: AtomicBool,
}
*/

fn update() {
  let mut rgb = GAMESTATE.rgb.try_lock().unwrap();
  fn increment_color(c: f32, amount_255: f32) -> f32 {
    let amount = (amount_255 / (u8::MAX as f32)).clamp(0.0, 1.0);
    let mut c = c + amount;
    if c > 1.0 {
      c -= 1.0;
    }
    c
  }

  rgb.r = increment_color(rgb.r, 2.0);
  rgb.g = increment_color(rgb.g, 3.0);
  rgb.b = increment_color(rgb.b, 5.0);

  let _ = GAMESTATE.x.fetch_add(1, SeqCst);
}

fn render() {
  let now = Instant::now();

  let ready = SWAP_BUFFERS.load(SeqCst);
  if ready {
    return;
  }

  let color;
  {
    let mutex = GAMESTATE.rgb.try_lock().unwrap();
    color = *mutex;
  }

  /*
  let rgb = GAMESTATE.rgb.load(SeqCst);
  let r = (rgb >> 16) & 0xFF;
  let g = (rgb >> 8) & 0xFF;
  let b = rgb & 0xFF;
  */

  let (window_width, window_height) = WINDOW_SIZE.load();
  let (orig_render_width, orig_render_height) = RENDER_SIZE.load();
  let mut render_width = orig_render_width;
  let mut render_height = orig_render_height;

  let render_nanos = RENDER_NANOS.load(SeqCst);
  let refresh_nanos = MONITOR_REFRESH_NANOS.load(SeqCst);

  if render_nanos >= refresh_nanos {
    render_width = (render_width * 11 / 16).max(1);
    render_height = (render_height * 11 / 16).max(1);
  } else if render_nanos < (refresh_nanos / 4) {
    render_width = (render_width * 11 / 8).min(window_width);
    render_height = (render_height * 11 / 8).min(window_height);
  }

  let update_render_size = render_width != orig_render_width
    || render_height != orig_render_height;
  if update_render_size {
    RENDER_SIZE.store(render_width, render_height);
  }

  BACKBUFFER.resize(render_width, render_height);

  let mut back_pixels = BACKBUFFER.pixels.try_lock().unwrap();
  let black = Color::rgb(0.0, 0.0, 0.0);

  // clear buffer
  for i in 0..render_width * render_height {
    back_pixels[i] = black;
  }

  // draw square
  for y in 1..render_height / 4 {
    for x in 1..render_width / 4 {
      back_pixels[y * render_width + x] = color;
    }
  }

  let game_x = GAMESTATE.x.load(SeqCst);
  back_pixels[((render_height * 3 / 4) * render_width) + game_x] =
    Color::rgb(0.0, 0.0, 1.0);
  back_pixels[((render_height * 3 / 4) * render_width) + game_x + 1] =
    Color::rgb(0.0, 0.0, 1.0);
  back_pixels[((render_height * 3 / 4) * render_width) + game_x + 2] =
    Color::rgb(0.0, 1.0, 0.0);
  back_pixels[((render_height * 3 / 4) * render_width) + game_x + 3] =
    Color::rgb(1.0, 0.0, 0.0);

  let buffer = &FRONTBUFFERS[spare_buffer_index()];
  buffer.resize(render_width, render_height);
  let mut front_pixels = buffer.pixels.try_lock().unwrap();

  for i in 0..render_width * render_height {
    front_pixels[i] = back_pixels[i].into();
  }

  drop(back_pixels);
  drop(front_pixels);

  SWAP_BUFFERS.store(true, SeqCst);

  let elapsed_nanos = now.elapsed().as_nanos().try_into().unwrap();
  RENDER_NANOS.store(elapsed_nanos, SeqCst);
}

fn main() -> Result<()> {
  unsafe {
    update_monitor_refresh_rate();
    // verify frame rate
    {
      let refresh_nanos = MONITOR_REFRESH_NANOS.load(SeqCst) as f64;
      let flush_count = 1_000_000_000.0 / refresh_nanos;

      DwmFlush().unwrap();
      let now = Instant::now();
      for i in 0..flush_count.round() as usize {
        DwmFlush().unwrap();
      }
      let elapsed = now.elapsed().as_secs_f64();

      let avg_frame_time_nanos = elapsed / flush_count * 1_000_000_000.0;
      let diff_nanos = (avg_frame_time_nanos - refresh_nanos).abs();
      assert!(diff_nanos < 10_000.0, "{diff_nanos}");
    }

    // start creating window
    let instance_handle = GetModuleHandleA(PCSTR::null())?;
    assert!(instance_handle.0 != 0);
    let window_class_name = s!("WindowClassName");
    let window_name = s!("ZInvaders");

    let window_class = WNDCLASSA {
      style: CS_HREDRAW | CS_VREDRAW | CS_OWNDC, // & WNDCLASS_STYLES(0),
      lpfnWndProc: Some(window_proc),
      hInstance: instance_handle.into(),
      lpszClassName: window_class_name,
      ..Default::default()
    };

    let atom = RegisterClassA(&window_class);
    assert!(atom != 0);

    {
      // new block to limit scope of hwnd in case we recreate the window
      // OpenGL requires WS_CLIPSIBLINGS and WS_CLIPCHILDREN
      let hwnd = CreateWindowExA(
        WINDOW_EX_STYLE::default(),
        window_class_name,
        window_name,
        WS_OVERLAPPEDWINDOW, // | WS_CLIPSIBLINGS | WS_CLIPCHILDREN,
        CW_USEDEFAULT,       // x
        CW_USEDEFAULT,       // y
        CW_USEDEFAULT,       // nWidth
        CW_USEDEFAULT,       // nHeight
        None,                // parent window handle
        None,                // menu handle
        instance_handle,     // instance
        None,                // lpParam
      );
      assert!(hwnd.0 != 0);
      WINDOW_HANDLE.store(hwnd.0, SeqCst);

      #[cfg(debug_assertions)]
      {
        SetWindowPos(
          hwnd,
          HWND(0),
          100,
          100,
          800,
          800,
          SET_WINDOW_POS_FLAGS(0),
        )
        .unwrap();
      }

      let hdc = GetDC(hwnd);
      assert!(hdc.0 != 0);

      // chose pixel format that supports OpenGL
      // for better fit, enumerate all of them and pick best one
      // instead of leaving it to Windows to choose
      let mut format_desc = PIXELFORMATDESCRIPTOR::default();
      format_desc.nSize = mem::size_of::<PIXELFORMATDESCRIPTOR>() as _;
      format_desc.nVersion = 1;
      format_desc.dwFlags = PFD_DRAW_TO_WINDOW; // PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
      format_desc.iPixelType = PFD_TYPE_RGBA;
      format_desc.cColorBits = 32;
      format_desc.cDepthBits = 16; // 16, 24 or 32?
                                   // format_desc.cStencilBits = 8;  // for OpenGL
      format_desc.iLayerType = PFD_MAIN_PLANE.0 as _;
      let format_id = ChoosePixelFormat(hdc, &format_desc);
      assert!(format_id > 0);

      // verify that we got a good pixel format
      let mut result_desc = PIXELFORMATDESCRIPTOR::default();
      let _max_format_id = DescribePixelFormat(
        hdc,
        format_id,
        mem::size_of::<PIXELFORMATDESCRIPTOR>() as _,
        Some(&mut result_desc),
      );
      assert!(
        (result_desc.dwFlags.0 & format_desc.dwFlags.0)
          == format_desc.dwFlags.0
      );
      assert!(
        (result_desc.iPixelType.0 & format_desc.iPixelType.0)
          == format_desc.iPixelType.0
      );
      assert!(result_desc.cColorBits >= format_desc.cColorBits);
      assert!(result_desc.cDepthBits >= format_desc.cDepthBits);
      assert!(
        result_desc.iLayerType == format_desc.iLayerType
          || result_desc.iLayerType == 0
      );

      SetPixelFormat(hdc, format_id, &result_desc).unwrap();

      /*
      let hglrc = wglCreateContext(hdc).unwrap();
      assert!(hglrc.0 != 0);
      wglMakeCurrent(hdc, hglrc).unwrap();
      */

      let mut client_rect = RECT::default();
      GetClientRect(hwnd, &mut client_rect).unwrap();
      let win_width = (client_rect.right - client_rect.left) as _;
      let win_height = (client_rect.bottom - client_rect.top) as _;
      WINDOW_SIZE.store(win_width, win_height);
      RENDER_SIZE.store(win_width, win_height);

      // let _ = EnableWindow(hwnd, true);
      let _ = ShowWindow(hwnd, SW_SHOW);
      // assert!(ValidateRect(hwnd, None).0 != 0);

      ReleaseDC(hwnd, hdc);
    }

    // draw before first update
    render();
    draw(true);

    let mut last_instant = Instant::now();
    let mut message = MSG::default();

    while SHOULD_RUN.load(Ordering::SeqCst) {
      while PeekMessageA(&mut message, None, 0, 0, PM_REMOVE).into() {
        if message.message == WM_KEYDOWN
          || message.message == WM_KEYUP
          || message.message == WM_SYSKEYUP
          || message.message == WM_SYSKEYDOWN
        {
          handle_keyboard_message(
            message.hwnd,
            message.message,
            message.wParam,
            message.lParam,
          );
        } else {
          DispatchMessageA(&message);
        }
      }

      update(); // TODO: make this use dt
      render();
      draw(true);

      // thread::sleep(Duration::from_millis(1));
      if false {
        let now = Instant::now();
        let frame_time = now.duration_since(last_instant);
        let (render_w, render_h) = RENDER_SIZE.load();
        let render_ms = RENDER_NANOS.load(SeqCst) as f64 / 1_000_000.0;
        println!(
          "Frame {} ms, front: {}, back: {}, render_size: {} x {} in {render_ms} ms",
          frame_time.as_secs_f32() * 1000.0,
          front_buffer_index(),
          spare_buffer_index(),
          render_w, render_h,
        );
        last_instant = now;
      }
    }

    let hglrc = wglGetCurrentContext();
    if hglrc.0 != 0 {
      let hdc = wglGetCurrentDC();
      assert!(hdc.0 != 0);
      wglMakeCurrent(HDC(0), HGLRC(0)).unwrap();
      let hwnd = WINDOW_HANDLE.load(SeqCst);
      ReleaseDC(HWND(hwnd), hdc);
      wglDeleteContext(hglrc).unwrap();
    }

    PostQuitMessage(0);
    Ok(())
  }
}

unsafe fn draw(wait_for_vsync: bool) {
  let hwnd = HWND(WINDOW_HANDLE.load(SeqCst));
  if hwnd.0 == 0 {
    println!("Got NULL hwnd. Skipping call to draw()");
    return;
  }

  let hdc = GetDC(hwnd);
  if hdc.0 == 0 {
    println!("Got NULL device context. Skipping call to draw()");
    return;
  }

  perform_buffer_swap();

  let buffer = &FRONTBUFFERS[front_buffer_index()];
  let (img_width, img_height) = buffer.size.load();
  let (win_width, win_height) = WINDOW_SIZE.load();

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

  if wait_for_vsync {
    // SwapBuffers(hdc).unwrap();
    DwmFlush().unwrap();
  }

  ReleaseDC(hwnd, hdc);
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
      draw(false);
      let _ = EndPaint(window, &paint);
      return LRESULT(0);
    } else if message == WM_KEYDOWN
      || message == WM_KEYUP
      || message == WM_SYSKEYDOWN
      || message == WM_SYSKEYUP
    {
      panic!("Should have handled keyboard messages in main loop");
    } else if message == WM_CLOSE || message == WM_DESTROY {
      SHOULD_RUN.store(false, Ordering::SeqCst);
      return LRESULT(0);
    } else if message == WM_ERASEBKGND {
      return LRESULT(1); // ignore it to avoid flicker
                         //
    } else if message == WM_QUIT {
      panic!("Got WM_QUIT. Windows should never send this message");
    } else if message == WM_SIZE {
      let width = lparam.0 & 0xFFFF;
      let height = (lparam.0 >> 16) & 0xFFFF;
      WINDOW_SIZE.store(width as _, height as _);
      RENDER_SIZE.store(width as _, height as _);
      return LRESULT(0);
    } else if message == WM_DISPLAYCHANGE {
      let _image_depth = wparam;
      let _horizontal_resolution = lparam.0 & 0xFFFF;
      let _vertical_resolution = (lparam.0 >> 16) & 0xFFFF;

      let mut client_rect = RECT::default();
      GetClientRect(window, &mut client_rect).unwrap();
      let win_width = (client_rect.right - client_rect.left) as _;
      let win_height = (client_rect.bottom - client_rect.top) as _;
      WINDOW_SIZE.store(win_width, win_height);
      RENDER_SIZE.store(win_width, win_height);

      update_monitor_refresh_rate();
    }
    DefWindowProcA(window, message, wparam, lparam)
  }
}

fn handle_keyboard_message(
  window: HWND,
  message: u32,
  wparam: WPARAM,
  lparam: LPARAM,
) {
  assert!(
    message == WM_KEYDOWN
      || message == WM_KEYUP
      || message == WM_SYSKEYDOWN
      || message == WM_SYSKEYUP
  );

  let key_code = VIRTUAL_KEY(wparam.0.try_into().unwrap());

  let lparam = lparam.0;
  let repeat_count = lparam & 0xFFFF; // 16 bits (0 - 15)
  let scan_code = (lparam >> 16) & 0xFF; // 8 bits (16 - 23)
  let is_extended = ((lparam >> 24) & 1) != 0; // 1 bit (24)
  let reserved = (lparam >> 25) & 0xF; // 4 bits (25 - 28)
  let context_code = (lparam >> 29) & 1; // 1 bit (29)
  let key_was_down = ((lparam >> 30) & 1) != 0; // 1 bit (30)
  let key_is_down = ((lparam >> 31) & 1) == 0; // 1 bit (31)

  // dbg!(key_code, is_extended, key_was_down, key_is_down);

  let repeat_key_down =
    key_was_down && (message == WM_KEYDOWN || message == WM_SYSKEYDOWN);
  let repeat_key_up =
    !key_was_down && (message == WM_KEYUP || message == WM_SYSKEYUP);

  if repeat_key_up {
    panic!("Got repeated key up event, which should never happen.");
  } else if repeat_key_down {
    // ignore reapeated presses
  } else if message == WM_KEYDOWN || message == WM_KEYUP {
    let mut state = true;
    if message == WM_KEYUP {
      state = false;
    }

    if key_code == VK_RIGHT {
      let prev = GAMESTATE.action_right.swap(state, SeqCst);
      assert!(prev != state);
    } else if key_code == VK_LEFT {
      let prev = GAMESTATE.action_left.swap(state, SeqCst);
      assert!(prev != state);
    } else if key_code == VK_UP {
      let prev = GAMESTATE.action_up.swap(state, SeqCst);
      assert!(prev != state);
    } else if key_code == VK_DOWN {
      let prev = GAMESTATE.action_down.swap(state, SeqCst);
      assert!(prev != state);
    } else if key_code == VK_SPACE {
      let prev = GAMESTATE.action_space.swap(state, SeqCst);
      assert!(prev != state);
    } else if key_code == VK_ESCAPE {
      SHOULD_RUN.store(false, SeqCst);
    } else {
      // println!("WARNING: Got unknown WM_KEYDOWN event");
    }

    // dbg!(&GAMESTATE);
  } else {
    // println!("WARNING: Got unhandled {message:?} message event");
  }
}
