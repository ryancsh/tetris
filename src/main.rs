#![allow(unused_imports)]
#![allow(unused_variables)]

use windows::core::*;
use windows::Win32::Foundation::*;
use windows::Win32::Graphics::Gdi::*;
use windows::Win32::UI::WindowsAndMessaging::*;
use windows::Win32::System::LibraryLoader::*;
use windows::Win32::UI::Input::KeyboardAndMouse::*;

use std::ptr;
use std::mem;
use std::sync::atomic::*;

static SHOULD_RUN : AtomicBool = AtomicBool::new(true);

pub fn rgb(r: u8, g: u8, b: u8) -> COLORREF {
  let r = (r as u32) << 16;
  let g = (g as u32) << 8;
  let b = b as u32;
  let result = b | g | r;
  COLORREF(result)
}

fn main() -> Result<()> {
  unsafe {
    let instance_handle = GetModuleHandleA(PCSTR::null())?;
    assert!(instance_handle.0 != 0);
    let window_class_name = s!("WindowClassName");
    let window_name = s!("Tetris");

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
      WS_OVERLAPPEDWINDOW | WS_DISABLED,    // window style
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

    let _ = EnableWindow(window_handle, true);
    let _ = ShowWindow(window_handle, SW_SHOW);

    // let brush = CreateSolidBrush(rgb(0xFF, 0, 0));
    // assert!(!brush.is_invalid());

    let mut message = MSG::default();
    while SHOULD_RUN.load(Ordering::SeqCst) {
      while PeekMessageA(&mut message, None, 0, 0, PM_REMOVE).into() {
        DispatchMessageA(&message);
      }
    }
    
    PostQuitMessage(0);
    Ok(())
  }
}

unsafe fn draw(hdc: HDC, rect: RECT) {
  let _test_image = [
    rgb(0xFF, 0x00, 0x00),
    rgb(0x00, 0xFF, 0x00),
    rgb(0x00, 0x00, 0xFF),
    rgb(0xFF, 0xFF, 0xFF),
  ];
  let mut header = BITMAPINFO::default();
  header.bmiHeader.biSize = mem::size_of_val(&header) as _;
  header.bmiHeader.biWidth = 2;
  header.bmiHeader.biHeight = 2;
  header.bmiHeader.biPlanes = 1;  // must be 1
  header.bmiHeader.biBitCount = 32;
  header.bmiHeader.biCompression = BI_RGB.0;

  let x = rect.left;
  let y = rect.top;
  let width = rect.right - rect.left;
  let height = rect.bottom - rect.top;
  
  let result = StretchDIBits(hdc, x, y, width, height, 0, 0, 2, 2, Some(image.as_ptr() as _), &header, DIB_RGB_COLORS, SRCCOPY);
  assert!(result != 0);
}

extern "system" fn window_proc(window: HWND, message: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
  unsafe {
    if message == WM_PAINT {
      let mut paint = PAINTSTRUCT::default();
      BeginPaint(window, &mut paint);

      let mut client_rect = RECT::default();
      GetClientRect(window, &mut client_rect).unwrap();

      draw(paint.hdc, client_rect);

      EndPaint(window, &paint);
    } else if message == WM_QUIT || message == WM_CLOSE || message == WM_DESTROY {
      SHOULD_RUN.store(false, Ordering::SeqCst);
    } else {
      return DefWindowProcA(window, message, wparam, lparam);
    }
    LRESULT(0)
  }
}
