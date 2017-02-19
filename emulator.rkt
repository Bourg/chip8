#lang racket

(define filename "roms/TANK")

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] regs [reg-i #:mutable]))

(define (load-program filename)
  ; First, read input into a byte string
  (define program-in-port (open-input-file filename))
  (define program-bytes (port->bytes program-in-port))
  (close-input-port program-in-port)

  ; Memory is 4096 bytes
  ; Start execution at PC 200
  ; Stack is a list
  ; Registers are 16 bytes
  (define state (chip8-state
                  (make-bytes #x1000)
                  #x200
                  '()
                  (make-bytes 16)
                  #x0
                  ))

  ; Move the program to memory address 0x200 in the emulator
  (bytes-copy! (chip8-state-memory state) #x200 program-bytes)

  state)

; This is the program state that will be modified 
(define state (load-program filename))

; Memory operations
(define (get-memory) (chip8-state-memory state))

; Stack operations
(define (pop-stack) (match (chip8-state-stack state)
                           ['() (error "cannot pop stack - no elements")]
                           [(cons h t) (set-chip8-state-stack! state t) h]))
(define (push-stack v) (set-chip8-state-stack! state (cons v (chip8-state-stack state))))

; Program counter operations
(define (get-pc) (chip8-state-pc state))
(define (set-pc v) (set-chip8-state-pc! state v))
(define (increment-pc) (set-pc (+ (get-pc) 2)))

(define (get-reg n) (bytes-ref (chip8-state-regs state) n))
(define (set-reg n v) (bytes-set! (chip8-state-regs state) n v))

(define (get-i) (chip8-state-reg-i state))
(define (set-i v) (set-chip8-state-reg-i! state v))

; Helpers to handle other operations
(define (clear-display) ("clear display"))

(define (take-step)
  (define current-instr
    (+ (* (bytes-ref (get-memory) (get-pc)) #x100)
       (bytes-ref (get-memory) (+ 1 (get-pc)))))

  (define (masked mask) (bitwise-and current-instr mask))

  (define (hex-form? mask value) (= (masked mask) value))

  (define x (get-reg (/ (masked #x0f00) #x100)))
  (define y (get-reg (/ (masked #x00f0) #x10)))
  (define kk (masked #x00ff))
  (define nnn (masked #x0fff))

  (printf "~x\n" current-instr)

  (cond
    [(= current-instr #x00e0) (increment-pc) (clear-display)]

    [(= current-instr #x00ee) (set-pc (pop-stack)) "pop from stack"]

    [(hex-form? #xf000 #x1000)
     (increment-pc)
     (set-pc nnn)
     "absolute jump"]

    [(hex-form? #xf000 #x2000)
     (increment-pc)
     (push-stack (get-pc))
     (set-pc nnn)
     "call"
     ]

    [(hex-form? #xf000 #x3000)
     (if (= (get-reg x) kk) (begin (increment-pc) (increment-pc)) (increment-pc))
     "jump reg value equal"
     ]

    [(hex-form? #xf000 #x4000)
     (if (= (get-reg x) kk) (increment-pc) (begin (increment-pc) (increment-pc)))
     "jump reg value not equal"
     ]

    [(hex-form? #xf00f #x5000)
     (if (= (get-reg x) (get-reg y)) (begin (increment-pc) (increment-pc)) (increment-pc))
     "jump reg reg equal"
     ]

    [(hex-form? #xf000 #x6000)
     (set-reg x kk)
     (increment-pc)
     "load register"]

    [(hex-form? #xf000 #x7000)
     (set-reg x (+ (get-reg x) kk))
     (increment-pc)
     "add to register"]

    [(hex-form? #xf00f #x8000)
     (set-reg x (get-reg y))
     (increment-pc)
     "set reg x = reg y"]

    [(hex-form? #xf00f #x8001)
     (set-reg x (bitwise-ior (get-reg x) (get-reg y)))
     (increment-pc)
     "bitwise or"]

    [(hex-form? #xf00f #x8002)
     (set-reg x (bitwise-and (get-reg x) (get-reg y)))
     (increment-pc)
     "bitwise and"]

    [(hex-form? #xf00f #x8003)
     (set-reg x (bitwise-xor (get-reg x) (get-reg y)))
     (increment-pc)
     "bitwise xor"]

    ; Addition
    [(hex-form? #xf00f #x8004)
     (let ([res (+ (get-reg x) (get-reg y))])
           (begin
             (set-reg x (modulo res #x100))
             (set-reg #xf (if (> res #xff) 1 0))))
     (increment-pc)
     "add"]

    ; Subtraction
    [(hex-form? #xf00f #x8005)
     (let ([res (- (get-reg x) (get-reg y))])
           (begin
             (set-reg x (modulo res #x100))
             (set-reg #xf (if (< res 0) 1 0))))
     (increment-pc)
     "subtract"]

    ; Divide
    [(hex-form? #xf00f #x8006)
     (set-reg #xf (bitwise-and #x1 (get-reg y)))
     (set-reg x (arithmetic-shift (get-reg y) (- 1)))
     (increment-pc)
     "divide"]

    ; Subtraction 2
    [(hex-form? #xf00f #x8007)
     (let ([res (- (get-reg y) (get-reg x))])
           (begin
             (set-reg x (modulo res #x100))
             (set-reg #xf (if (< res 0) 1 0))))
     (increment-pc)
     "subtract backwards"]

    ; Multiplication
    [(hex-form? #xf00f #x800e)
     (set-reg #xf (bitwise-and #x80 (get-reg y)))
     (set-reg x (arithmetic-shift (get-reg y) 1))
     (increment-pc)
     "multiply"]

    [(hex-form? #xf00f #x9000)
     (if (= (get-reg x) (get-reg y)) (increment-pc) (begin (increment-pc) (increment-pc)))
     (increment-pc)
     "jump reg reg not equal"
     ]

    [(hex-form? #xf000 #xa000)
     (set-i nnn)
     (increment-pc)
     "load reg i"]

    [(hex-form? #xf000 #xb000)
     (set-pc (+ nnn (get-reg 0)))
     "jump by val of reg 0 plus nnn"]

    [(hex-form? #xf000 #xc000)
     (set-reg x (bitwise-and (random 256) kk))
     "random number into register"]

    ; Dxyn - Graphics
    ; Ex9E - Graphics
    ; ExA1 - Graphics
    ; Fx07 - Load delay timer
    ; Fx0A - Graphics
    ; Fx15 - Set delay timer
    ; Fx18 - Set sound timer
    ; Fx1E - Addition with I
    ; Fx29 - Load sprite memory address
    ; Fx33 - Store VX in weird representation
    ; Fx55 - Store registers in memory
    ; Fx65 - Read registers from memory

    [else (increment-pc) "unrecognized command"]
    )
  )

(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
(display (string-append (take-step) "\n"))
