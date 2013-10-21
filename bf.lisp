(defpackage #:bf
  (:use #:cl)
  (:export #:run
           #:run-file))

(in-package #:bf)

(declaim (ftype function
                move-ip-to-]
                move-ip-to-[))

(defconstant +data-size+ 30000)

(defstruct bf
  (instructions "" :type string)
  (ip 0 :type fixnum)
  (data (make-array +data-size+
                    :element-type '(unsigned-byte 8)
                    :initial-element 0))
  (dp 0 :type fixnum))

(defun read-instruction (bf)
  (aref (bf-instructions bf) (bf-ip bf)))

(defun handle-> (bf)
  (incf (bf-dp bf)))

(defun handle-< (bf)
  (decf (bf-dp bf)))

(defun handle-+ (bf)
  (let ((new-value (1+ (aref (bf-data bf) (bf-dp bf)))))
    (setf (aref (bf-data bf) (bf-dp bf))
          (mod new-value 256))))

(defun handle-- (bf)
  (let ((new-value (1- (aref (bf-data bf) (bf-dp bf)))))
    (setf (aref (bf-data bf) (bf-dp bf))
          (mod new-value 256))))

(defun handle-. (bf)
  (write-char (code-char (aref (bf-data bf)
                               (bf-dp bf)))))

(defun handle-comma (bf)
  (let ((b (read-char *standard-input* nil 0)))
    (setf (aref (bf-data bf) (bf-dp bf))
          (char-code b))))

(defun handle-[ (bf)
  (let ((b (aref (bf-data bf) (bf-dp bf))))
    (if (zerop b)
        (progn
          (incf (bf-ip bf))
          (move-ip-to-] bf)))))

(defun move-ip-to-] (bf)
  (let ((i (aref (bf-instructions bf) (bf-ip bf))))
    (case i
      (#\[ (handle-[ bf))
      (#\] (return-from move-ip-to-])))
    (incf (bf-ip bf))
    (move-ip-to-] bf)))

(defun handle-] (bf)
  (let ((b (aref (bf-data bf) (bf-dp bf))))
    (unless (zerop b)
      (decf (bf-ip bf))
      (move-ip-to-[ bf))))

(defun move-ip-to-[ (bf)
  (let ((i (aref (bf-instructions bf) (bf-ip bf))))
    (case i
      (#\] (handle-] bf))
      (#\[ (return-from move-ip-to-[)))
    (decf (bf-ip bf))
    (move-ip-to-[ bf)))

(defun run (instructions)
  (let ((bf (make-bf :instructions instructions)))
    (labels ((run-helper (bf)
               (if (< (bf-ip bf) (length (bf-instructions bf)))
                   (let ((i (read-instruction bf)))
                     (case i
                       (#\> (handle-> bf))
                       (#\< (handle-< bf))
                       (#\+ (handle-+ bf))
                       (#\- (handle-- bf))
                       (#\. (handle-. bf))
                       (#\, (handle-comma bf))
                       (#\[ (handle-[ bf))
                       (#\] (handle-] bf)))
                     (incf (bf-ip bf))
                     (run-helper bf)))))
      (run-helper bf))))

(defun run-file (path)
  (let ((program (with-open-file (in path :direction :input)
                   (let ((pgm (make-string (file-length in))))
                     (read-sequence pgm in)
                     pgm))))
    (run program)))
