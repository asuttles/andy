   ;; WASI IO RUNTIME  ────────────────────────────────────────────────────

   (data (i32.const 0)
     "\0A"                 ;; 1   ESC_NEWLINE
     "\1b[2J\1b[H"         ;; 7   ESC_CLEAR_SCREEN
     "\1b[H"               ;; 3   ESC_CURSOR_HOME
     "\1b[?25l"            ;; 6   ESC_HIDE_CURSOR
     "\1b[?25h"            ;; 6   ESC_SHOW_CURSOR
     "\1b[0m"              ;; 4   ESC_RESET_STYLE
     "\1b[1m"              ;; 4   ESC_BOLD
     "\1b[3m"              ;; 4   ESC_ITALIC
     "\1b[4m"              ;; 4   ESC_UNDERLINE
     "\1b[30m"             ;; 5   ESC_BLACK_FG
     "\1b[31m"             ;; 5   ESC_RED_FG
     "\1b[32m"             ;; 5   ESC_GREEN_FG
     "\1b[33m"             ;; 5   ESC_YELLOW_FG
     "\1b[34m"             ;; 5   ESC_BLUE_FG
     "\1b[35m"             ;; 5   ESC_MAGENTA_FG
     "\1b[36m"             ;; 5   ESC_CYAN_FG
     "\1b[37m"             ;; 5   ESC_WHITE_FG
     "\1b[40m"             ;; 5   ESC_BLACK_BG
     "\1b[41m"             ;; 5   ESC_RED_BG
     "\1b[42m"             ;; 5   ESC_GREEN_BG
     "\1b[43m"             ;; 5   ESC_YELLOW_BG
     "\1b[44m"             ;; 5   ESC_BLUE_BG
     "\1b[45m"             ;; 5   ESC_MAGENTA_BG
     "\1b[46m"             ;; 5   ESC_CYAN_BG
     "\1b[47m"             ;; 5   ESC_WHITE_BG
     "\1b[K"               ;; 3   ESC_CLEAR_LINE_FROM_CURSOR
     "\1b[2K"              ;; 4   ESC_CLEAR_LINE
     "\1b]0;TITLE\07"      ;; 9   ESC_SET_TITLE
    )

   ;; Constants for runtime headers and buffers
   (global $runtime_headers i32 (i32.const 2048))
   (global $runtime_buffers i32 (i32.const 2304))
   
   
   ;; Write a string (ptr, len)
   (func $write_string (param $ptr i32) (param $len i32)
     (local $iov_ptr i32)
     ;; iovec at runtime_headers
     (local.set $iov_ptr (global.get $runtime_headers))
   
     ;; iov_base = ptr
     (i32.store (local.get $iov_ptr) (local.get $ptr))
     ;; iov_len = len
     (i32.store (i32.add (local.get $iov_ptr) (i32.const 4)) (local.get $len))
   
     ;; call fd_write(fd=1, &iovec, 1, &nwritten)
     (call $fd_write
       (i32.const 1)                          ;; fd = stdout
       (local.get $iov_ptr)                   ;; pointer to iovec array
       (i32.const 1)                          ;; number of iovecs
       (i32.add (local.get $iov_ptr) (i32.const 8)) ;; where to write nwritten
     )
     drop
   )
   
   ;; export a newline printer
    (func $write_newline
      (call $write_string (i32.const 0) (i32.const 1)))
  
    ;; Write an i32 value
    ;; This version writes digits to runtime_buffers as ASCII characters.
    (func $write_i32 (param $n i32)
      (local $buf i32) (local $p i32) (local $digit i32)
      (local.set $buf (global.get $runtime_buffers))
      (local.set $p (i32.add (local.get $buf) (i32.const 32))) ;; end of buffer
  
      (if (i32.eqz (local.get $n))
          (then
            (i32.store8 (i32.sub (local.get $p) (i32.const 1)) (i32.const 48))
            (call $write_string (i32.sub (local.get $p) (i32.const 1)) (i32.const 1))
          )
          (else
            (loop $digits
              (local.set $digit (i32.rem_u (local.get $n) (i32.const 10)))
              (local.set $n (i32.div_u (local.get $n) (i32.const 10)))
              (i32.store8
                (i32.sub (local.get $p) (i32.const 1))
                (i32.add (local.get $digit) (i32.const 48)))
              (local.set $p (i32.sub (local.get $p) (i32.const 1)))
              (br_if $digits (i32.gt_u (local.get $n) (i32.const 0)))
            )
            ;; Write digits from p to end of buffer
            (call $write_string (local.get $p)
                                (i32.sub
                                  (i32.add (local.get $buf) (i32.const 32))
                                  (local.get $p)))
          )
      )
    )
   
    ;; Write a f64 value (approximate, prints 6 digits after the decimal point)
    (func $write_f64 (param $x f64)
      (local $intpart i32)
      (local $fracpart f64)
      (local $scaled i32)
    
      ;; Handle negative numbers
      (if (f64.lt (local.get $x) (f64.const 0))
          (then
            ;; print '-'
            (i32.store8 (global.get $runtime_buffers) (i32.const 45)) ;; '-'
            (call $write_string (global.get $runtime_buffers) (i32.const 1))
            (local.set $x (f64.neg (local.get $x)))
          )
      )
    
      ;; integer part
      (local.set $intpart (i32.trunc_f64_s (local.get $x)))
      (call $write_i32 (local.get $intpart))
    
      ;; print '.'
      (i32.store8 (global.get $runtime_buffers) (i32.const 46)) ;; '.'
      (call $write_string (global.get $runtime_buffers) (i32.const 1))
    
      ;; fractional part * 1_000_000
      (local.set $fracpart
        (f64.sub (local.get $x) (f64.convert_i32_s (local.get $intpart))))
      (local.set $fracpart
        (f64.mul (local.get $fracpart) (f64.const 1000000)))
      (local.set $scaled (i32.trunc_f64_s (local.get $fracpart)))
    
      ;; print fractional digits (no zero padding)
      (call $write_i32 (local.get $scaled))
    )
  
   ;; END WASI IO RUNTIME  ────────────────────────────────────────────────

