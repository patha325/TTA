;; interaction-utilities.scm

;; Bugfix 2005-10-11: Fixed reading of lines ending with space + enter.
;; And again 2005-08-06. Removed dependency on tables, enter-new-command!
;; is a closure.
;; Reimplemented by Joakim Hellsten, 2005-05-06

;; This package provides some procdures for reading a line of 
;; text, especially in an interaction loop when the first token (word)
;; is a command, and the rest of the line is its arguments. 

;: This file provides the following for entering commands:
;; (enter-new-command!) - ask the user for his next command, 
;; (get-command-name) - get the <name> part for the last command.
;; (get-command-arguments) - get a list of the <arguments>

;; The following procedure is included for simple output:
;; (display* arg1 arg2 ... argN) - display any number of tokens.

;; A "command" is of the form <name> <arguments>.
;; <name> should be a single token (word), <arguments> are an arbitrary
;; number tokens separated by space.

;; Example:
;; (enter-new-command!) -> <void>
;; >> put coin in machine
;; (get-command-name) -> put
;; (get-command-arguments) -> (coin in machine)

;; > (display* "foo " 'fie " " 333 "\n" 1 " two " 'three)
;; foo fie 333
;; 1 two three

(define enter-new-command!
  (let ((command #f)
        (arguments #f))
    
    (define (read-arguments)
      (let ((next-char (peek-char)))
        (cond ((eq? #\newline next-char)
               '())
              ((eq? #\space next-char)
               ;; consume spaces until something else is found.
               (read-char) 
               (read-arguments))
              ((eof-object? next-char)
               (set! command eof))
              (else
               (cons (read) (read-arguments))))))
    
    (define (get-command-name)
      (if command
          command
          (error "You have to create the command first with enter-new-command!")))
    
    (define (get-command-arguments)
      (if arguments
          arguments
          (error "You have to create the command first with enter-new-command!")))
    
    (lambda msg
      (if (> (length msg) 0) 
          (cond 
            ((eq? (car msg) 'get-command-name) get-command-name)
            ((eq? (car msg) 'get-command-arguments) get-command-arguments)
            (else #f))
          (begin
            (set! command (read))
            (set! arguments (read-arguments)))))))


;; Get the name of the command, that is, the first token of the 
;; command string.
(define (get-command-name)
  ((enter-new-command! 'get-command-name)))

;; Get the arguments for a command. Everything following the first
;; token of the command line.
(define (get-command-arguments)
  ((enter-new-command! 'get-command-arguments)))

;; Simplified display, displays any number of objects.
(define (display* . tokens) 
  (for-each display tokens))