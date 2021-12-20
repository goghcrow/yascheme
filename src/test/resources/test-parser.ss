(lambda (add a b) (+ a b))
(let ([a 1] [b 2])
  (add a b))

(define z 0)
(define a '(1 2 3))
  `(,z ,@a)

(
  ; 字符串
  [
    "Hello World"
    ; unicode
    "\u4e2d\t\u56fd" "中\t国"
    ; 转义
    "\r\n\b\f"
    "\""
    "\\"
    ; quote
      '"\""
  ]


  0 ; int

  (1b 1B -1b +1B) ; byte
  (2s 2S -2s +2S) ; short
  (3l 3L -3l +3L) ; long
  (3.14f 3.14F -3.14f +3.14F) ; float
  (3.14d 3.14D -3.14d +3.14D) ; double
  (6n 6N -6n +6N) ; bitint
  (3.14m 3.14M -3.14m +3.14M) ; bigdec

  (-3.14 3.14 +3.14) ; double
  (-42 42 +42); int

  (
    ; 2进制
    (
      0b1010101 ; int
      -0b1010101l ; long
      +0b1010101L ;long
    )

    ; 16 进制
    (
      -0xb 0xf +0xd ;

      0xabcd ; int
      -0xabcdl ; long
      +0xabcdL ; long
    )

    ; 8进制
    (
      0o1234567 ; int
      -0o1234567l ; long
      +0o1234567L ; long
    )
  )

  ; bool
  (#t #f)

  ; symbol
  sym
)

