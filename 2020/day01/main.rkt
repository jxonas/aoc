#lang racket


;; Method 1

(define (simple-but-slow target numbers n)
  (for/first ([c (in-combinations numbers n)]
              #:when (= target (apply + c)))
    c))



;; Method 2: performatic and complex...

(require (for-syntax racket/list))

(define (apply-op-to-vec vec op . idxs)
  (apply op (map (curry vector-ref vec) idxs)))

(define-syntax (performatic-and-more-complex stx)
  (syntax-case stx ()
    [(_ target numbers ns)
     (let ([ids (for/list ([i (in-range (syntax->datum #'ns))])
                  (string->symbol (format "idx~a" (add1 i))))])
       (with-syntax ([(idx ...) ids]
                     [(start ...) #`(-1 #,@(drop-right ids 1))])
         #`(let ([vec (list->vector (sort numbers <))]
                 [n (length numbers)])
             (for*/first ([idx (in-range (add1 start) n)]
                          ...
                          #:when (= target (apply-op-to-vec vec + idx ...)))
               (apply-op-to-vec vec list idx ...)))))]))


;; Helper


(define (input->numbers path)
  (call-with-input-file path
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))


(define (print-solution n nums)
  (if nums
      (displayln (format "~a = ~a ~a" n (apply * nums) nums))
      (displayln (format "~a = no solution found" n))))



(module+ main
  (define nums (input->numbers "input.txt"))
  (time
   (displayln "Method 1: slower...")
   (print-solution 2 (simple-but-slow 2020 nums 2))
   (print-solution 3 (simple-but-slow 2020 nums 3))
   (print-solution 4 (simple-but-slow 2020 nums 4)))
  (time
   (displayln "Method 2: faster!")
   (print-solution 2 (performatic-and-more-complex 2020 nums 2))
   (print-solution 3 (performatic-and-more-complex 2020 nums 3))
   (print-solution 4 (performatic-and-more-complex 2020 nums 4))))
