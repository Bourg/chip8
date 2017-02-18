#lang racket

(define filename "roms/TANK")

(struct chip8-state (memory [pc #:mutable] [stack #:mutable] regs))

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
                  (make-bytes 16)))

  (bytes-copy! (chip8-state-memory state) #x200 program-bytes)

  state
  )

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

; Helpers to handle other operations
(define (clear-display) (printf "> clear display\n"))

(define (take-step)
  (define current-instr
    (+ (* (bytes-ref (get-memory) (get-pc)) #x100)
       (bytes-ref (get-memory) (+ 1 (get-pc)))))

  (define (masked mask) (bitwise-and current-instr mask))
  (define (hex-form? mask value) (= (masked mask) value))


  (printf "~x\n" current-instr)

  (cond
    [(= current-instr #x00e0) (clear-display) (increment-pc)]

    [(= current-instr #x00ee) (set-pc (pop-stack))]

    [(hex-form? #xf000 #x1000)
     (increment-pc)
     (set-pc (masked #x0fff))]

    [(hex-form? #xf000 #x2000)
     (increment-pc)
     (push-stack (get-pc))
     (set-pc (masked #x0fff))
     ]

    [(hex-form? #xf000 #x3000)
     (let (
           [reg-val (get-reg (/ (masked #x0f00) #x100))]
           [cmp-val (bitwise-and current-instr #x00ff)])

       (if (= reg-val cmp-val) (begin (increment-pc) (increment-pc)) (increment-pc)))
     ]

    [(hex-form? #xf000 #x4000)
     (let (
           [reg-val (get-reg (/ (masked #x0f00) #x100))]
           [cmp-val (bitwise-and current-instr #x00ff)])

       (if (= reg-val cmp-val) (increment-pc) (begin (increment-pc) (increment-pc))))
     ]


    )
  )

(take-step)
(clear-display)
(clear-display)
