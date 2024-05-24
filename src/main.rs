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

static SHOULD_RUN: AtomicBool = AtomicBool::new(true);
static WINDOW_HANDLE: AtomicIsize = AtomicIsize::new(0);

static MONITOR_REFRESH_RATE: AtomicU32 = AtomicU32::new(60);
unsafe fn update_monitor_refresh_rate() {
  let mut devmode = DEVMODEA::default();
  devmode.dmSize = mem::size_of_val(&devmode) as _;
  let result =
    EnumDisplaySettingsA(None, ENUM_CURRENT_SETTINGS, &mut devmode);
  let refresh_rate = devmode.dmDisplayFrequency;
  if result.0 != 0 && refresh_rate > 1 {
    MONITOR_REFRESH_RATE.store(refresh_rate, SeqCst);
  }
}

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
static SWAP_BUFFERS: AtomicBool = AtomicBool::new(false);
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

#[derive(Debug)]
struct GameState {
  pub rgb: AtomicUsize,
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
      rgb: AtomicUsize::new(0),
      x: AtomicUsize::new(0),
      action_up: AtomicBool::new(false),
      action_down: AtomicBool::new(false),
      action_left: AtomicBool::new(false),
      action_right: AtomicBool::new(false),
      action_space: AtomicBool::new(false),
    }
  }
}

struct KeyState {
  pub ended_down: AtomicBool,
}

fn update() {
  let rgb = GAMESTATE.rgb.load(SeqCst);
  let r = ((rgb & 0xFF_0000) + (2 << 16)) & 0xFF_0000;
  let g = ((rgb & 0x00_FF00) + (3 << 8)) & 0x00_FF00;
  let b = ((rgb & 0x00_00FF) + 5) & 0xFF;
  let new_rgb = r | g | b;
  GAMESTATE.rgb.compare_exchange(rgb, new_rgb, SeqCst, SeqCst).unwrap();

  let _ = GAMESTATE.x.fetch_add(1, SeqCst);
}

fn render() {
  let ready = SWAP_BUFFERS.load(SeqCst);
  if ready {
    return;
  }

  let rgb = GAMESTATE.rgb.load(SeqCst);
  let r = (rgb >> 16) & 0xFF;
  let g = (rgb >> 8) & 0xFF;
  let b = rgb & 0xFF;

  let back_buffer = &FRAMEBUFFERS[back_buffer_index()];

  let (win_width, win_height) = TARGET_WINDOW_SIZE.load();
  back_buffer.resize(win_width, win_height);

  let mut pixels = back_buffer.pixels.try_lock().unwrap();
  let black = rgb_to_colorref(0, 0, 0);
  for i in 0..win_width * win_height {
    pixels[i] = black;
  }

  let color = rgb_to_colorref(r as _, g as _, b as _);
  for y in 1..win_height / 4 {
    for x in 1..win_width / 4 {
      pixels[y * win_width + x] = color;
    }
  }

  let game_x = GAMESTATE.x.load(SeqCst);
  pixels[((win_height * 3 / 4) * win_width) + game_x] =
    rgb_to_colorref(0, 0, 0xFF);
  pixels[((win_height * 3 / 4) * win_width) + game_x + 1] =
    rgb_to_colorref(0, 0, 0xFF);
  pixels[((win_height * 3 / 4) * win_width) + game_x + 2] =
    rgb_to_colorref(0, 0xFF, 0);
  pixels[((win_height * 3 / 4) * win_width) + game_x + 3] =
    rgb_to_colorref(0xFF, 0, 0);

  SWAP_BUFFERS.store(true, SeqCst);
}

fn main() -> Result<()> {
  unsafe {
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
        WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN,
        CW_USEDEFAULT,   // x
        CW_USEDEFAULT,   // y
        CW_USEDEFAULT,   // nWidth
        CW_USEDEFAULT,   // nHeight
        None,            // parent window handle
        None,            // menu handle
        instance_handle, // instance
        None,            // lpParam
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
      TARGET_WINDOW_SIZE.store(
        (client_rect.right - client_rect.left) as _,
        (client_rect.bottom - client_rect.top) as _,
      );

      update_monitor_refresh_rate();

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
        println!(
          "Frame {} ms, front: {}, back: {}",
          frame_time.as_secs_f32() * 1000.0,
          front_buffer_index(),
          back_buffer_index(),
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
      TARGET_WINDOW_SIZE.store(width as _, height as _);
      return LRESULT(0);
    } else if message == WM_DISPLAYCHANGE {
      let _image_depth = wparam;
      let _horizontal_resolution = lparam.0 & 0xFFFF;
      let _vertical_resolution = (lparam.0 >> 16) & 0xFFFF;

      let mut client_rect = RECT::default();
      GetClientRect(window, &mut client_rect).unwrap();
      TARGET_WINDOW_SIZE.store(
        (client_rect.right - client_rect.left) as _,
        (client_rect.bottom - client_rect.top) as _,
      );

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

    dbg!(&GAMESTATE);
  } else {
    // println!("WARNING: Got unhandled {message:?} message event");
  }
}
