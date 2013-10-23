(defpackage #:bf
  (:use #:cl)
  (:export #:run
           #:run-file
           #:main))

(in-package #:bf)

(declaim (ftype function
                move-ip-to-]
                move-ip-to-[))

(defconstant +data-size+ 30000)

(defstruct bf
  (instructions "" :type string)
  (ip -1 :type fixnum)
  (data (make-array +data-size+
                    :element-type '(unsigned-byte 8)
                    :initial-element 0))
  (dp 0 :type fixnum))

(defun next-instruction (bf)
  (if (>= (bf-ip bf)
          (1- (length (bf-instructions bf))))
      nil
      (progn
        (incf (bf-ip bf))
        (aref (bf-instructions bf)
              (bf-ip bf)))))

(defun prev-instruction (bf)
  (if (<= (bf-ip bf) 0)
      nil
      (progn
        (decf (bf-ip bf))
        (aref (bf-instructions bf)
              (bf-ip bf)))))

(defun read-data (bf)
  (aref (bf-data bf)
        (bf-dp bf)))

(defun write-data (data bf)
  (setf (aref (bf-data bf)
              (bf-dp bf))
        data))

(defun incr-data (bf)
  (let ((new-val (1+ (read-data bf))))
    (write-data (mod new-val 256)
                bf)))

(defun decr-data (bf)
  (let ((new-val (1- (read-data bf))))
    (write-data (mod new-val 256)
                bf)))

(defun dp-> (bf)
  (incf (bf-dp bf)))

(defun <-dp (bf)
  (decf (bf-dp bf)))

(defun handle-> (bf)
  (dp-> bf))

(defun handle-< (bf)
  (<-dp bf))

(defun handle-+ (bf)
  (incr-data bf))

(defun handle-- (bf)
  (decr-data bf))

(defun handle-. (bf)
  (write-char (code-char (read-data bf))))

(defvar *char-buf* nil)

(defun fill-char-buf ()
  (setf *char-buf* (nreverse *char-buf*))
  (multiple-value-bind (line newline-missing)
      (read-line *standard-input* nil :eof)
    (if (eql line :eof)
        (push :eof *char-buf*)
        (progn
          (map-into line
                    #'(lambda (c)
                        (push c *char-buf*))
                    line)
          (unless newline-missing
            (push #\Newline *char-buf*))))
    (setf *char-buf* (nreverse *char-buf*))))
    
(defun read-char-buf ()
  (let ((c (pop *char-buf*)))
    (or c
        (progn
          (fill-char-buf)
          (pop *char-buf*)))))

(defun handle-comma (bf)
  (let ((b (read-char-buf)))
    (unless (eql b :eof)
      (write-data (char-code b)
                  bf))))

(defun handle-[ (bf)
  (let ((b (read-data bf)))
    (if (zerop b)
        (move-ip-to-] bf))))

(defun move-ip-to-] (bf)
  (let ((i (next-instruction bf)))
    (if i
        (case i
          (#\[ (move-ip-to-] bf)
               (move-ip-to-] bf))
          (#\] (return-from move-ip-to-]))
          (otherwise (move-ip-to-] bf))))))

(defun handle-] (bf)
  (let ((b (read-data bf)))
    (unless (zerop b)
      (move-ip-to-[ bf))))

(defun move-ip-to-[ (bf)
  (let ((i (prev-instruction bf)))
    (if i
        (case i
          (#\] (move-ip-to-[ bf)
               (move-ip-to-[ bf))
          (#\[ (return-from move-ip-to-[))
          (otherwise (move-ip-to-[ bf))))))

(defun run (instructions)
  (let ((bf (make-bf :instructions instructions)))
    (labels ((run-loop (bf)
               (let ((i (next-instruction bf)))
                 (when i
                   (case i
                     (#\> (handle-> bf))
                     (#\< (handle-< bf))
                     (#\+ (handle-+ bf))
                     (#\- (handle-- bf))
                     (#\. (handle-. bf))
                     (#\, (handle-comma bf))
                     (#\[ (handle-[ bf))
                     (#\] (handle-] bf)))
                   (run-loop bf)))))
      (run-loop bf))))

(defun run-file (path)
  (let ((program (with-open-file (in path :direction :input)
                   (let ((pgm (make-string (file-length in))))
                     (read-sequence pgm in)
                     pgm))))
    (run program)))

(defun main ()
  (if (< (length sb-ext:*posix-argv*) 2)
      (error "Program file must be provided."))
  (run-file (second sb-ext:*posix-argv*)))
