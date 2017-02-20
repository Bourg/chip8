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
  
  ; Load the built in sprites - decimals
  (bytes-copy! (chip8-state-memory state) 0 #"\360\220\220\220\360")
  (bytes-copy! (chip8-state-memory state) 5 #"\040\140\040\040\160")
  (bytes-copy! (chip8-state-memory state) 10 #"\360\020\360\200\360")
  (bytes-copy! (chip8-state-memory state) 15 #"\360\020\360\020\360")
  (bytes-copy! (chip8-state-memory state) 20 #"\220\220\360\020\020")
  (bytes-copy! (chip8-state-memory state) 25 #"\360\200\360\020\360")
  (bytes-copy! (chip8-state-memory state) 30 #"\360\200\360\220\360")
  (bytes-copy! (chip8-state-memory state) 35 #"\360\020\040\200\200")
  (bytes-copy! (chip8-state-memory state) 40 #"\360\220\360\220\360")
  (bytes-copy! (chip8-state-memory state) 45 #"\360\220\360\020\360")
  ; And now hex
  (bytes-copy! (chip8-state-memory state) 50 #"\360\220\360\220\220")
  (bytes-copy! (chip8-state-memory state) 55 #"\340\220\340\220\340")
  (bytes-copy! (chip8-state-memory state) 60 #"\360\200\200\200\360")
  (bytes-copy! (chip8-state-memory state) 65 #"\340\220\220\220\340")
  (bytes-copy! (chip8-state-memory state) 70 #"\360\200\360\200\360")
  (bytes-copy! (chip8-state-memory state) 75 #"\360\200\360\200\200")

  state)

; This is the program state that will be modified 
(define state (load-program filename))

; Memory operations
(define memory (chip8-state-memory state))

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

; Timer stuff
(struct timer-info ([value #:mutable] [last-set #:mutable]))

(define delay-timer (timer-info 0 0))
(define sound-timer (timer-info 0 0))

(define (set-timer! timer time)
  (set-timer-info-value! timer time)
  (set-timer-info-last-set! timer (current-milliseconds)))
(define (tick-timer! timer)
  (when (>= (- (current-milliseconds) (timer-info-last-set timer)) 1000)
    (begin
      (set-timer-info-value! timer (- (timer-info-value timer) 1))
      (set-timer-info-last-set! timer (current-milliseconds)))))
(define (tick-timers!)
  (tick-timer! delay-timer)
  (tick-timer! sound-timer))
(define (timer-active? timer) (> (timer-info-value timer) 0))


; Helpers to handle other operations
(define (clear-display) ("clear display"))

(define (take-step)
  (define current-instr
    (+ (* (bytes-ref memory (get-pc)) #x100)
       (bytes-ref memory (+ 1 (get-pc)))))

  (define (masked mask) (bitwise-and current-instr mask))

  (define (hex-form? mask value) (= (masked mask) value))

  (define x (get-reg (/ (masked #x0f00) #x100)))
  (define y (get-reg (/ (masked #x00f0) #x10)))
  (define kk (masked #x00ff))
  (define nnn (masked #x0fff))

  (printf "~x\n" current-instr)
  (tick-timers!)

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
     (set-reg x (modulo (arithmetic-shift (get-reg y) 1) #x100))
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
     (increment-pc)
     "random number into register"]

    ; Dxyn - Graphics

    ; Ex9E - Keypress jump
    ; ExA1 - Keypress jump

    ; Load delay timer
    [(hex-form? #xf0ff #xf007)
     (set-reg x (timer-info-value delay-timer))
     (increment-pc)
     "load delay timer"]

    ; Fx0A - Keypress block

    ; Set delay timer
    [(hex-form? #xf0ff #xf015)
     (set-timer! delay-timer (get-reg x))
     (increment-pc)
     "set delay timer"]

    ; Set sound timer
    [(hex-form? #xf0ff #xf018)
     (set-timer! sound-timer (get-reg x))
     (increment-pc)
     "set sound timer"]

    ; Addition with I
    [(hex-form? #xf0ff #xf01e)
     (set-i (modulo (+ (get-i) (get-reg x)) #x1000))
     (increment-pc)
     "add to reg i"]

    ; Load prebake sprite memory address
    [(hex-form? #xf0ff #xf029)
     (set-i (* 5 (get-reg x)))
     (increment-pc)
     "load prebake sprite"]

    ; Store BCD
    [(hex-form? #xf0ff #xf033)
     (bytes-set! memory (get-i) (truncate (/ (get-reg x) 100)))
     (bytes-set! memory (+ 1 (get-i)) (truncate (/ (modulo (get-reg x) 100) 10)))
     (bytes-set! memory (+ 2 (get-i)) (modulo (get-reg x) 10))
     (increment-pc)
     "store fx in weird form (BCD)"]

    ; Store registers in memory
    [(hex-form? #xf0ff #xf055)
     (for ([i (+ x 1)]) (bytes-set! memory (+ (get-i) i) (get-reg i)))
     (increment-pc)
     "store registers"]

    ; Read registers from memory
    [(hex-form? #xf0ff #xf065)
     (for ([i (+ x 1)]) (set-reg i (bytes-ref memory (+ (get-i) i))))
     (increment-pc)
     "load registers"]

    [else (increment-pc) "unrecognized command"]
    )
  )

(for ([i 100]) (display (string-append (take-step) "\n")))
