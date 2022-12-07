#!/usr/bin/clisp
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defstruct command
  command-type
  input
  output)

(defstruct mydir
  name
  children)

(defstruct myfile
  name
  size)

(defstruct fs
  cwd
  root)

(defun split-stuff (l)
  (let ((pos (position #\SPACE l)))
    (list (subseq l 0 pos)
          (subseq l (+ pos 1)))))

(defun mkdir (d p name)
  (if (null p)
    (make-mydir
      :name (mydir-name d)
      :children (cons (make-mydir :name name) (mydir-children d)))
    (make-mydir
      :name (mydir-name d)
      :children (mapcar
                  (lambda (x) (cond ((typep x 'myfile) x)
                                    ((typep x 'mydir) (if (equal (mydir-name x) (first p))
                                                        (mkdir x (rest p) name)
                                                        x))))
                  (mydir-children d)))))

(defun touch (d p name size)
  (if (null p)
    (make-mydir
      :name (mydir-name d)
      :children (cons (make-myfile :name name :size size) (mydir-children d)))
    (make-mydir :name (mydir-name d) :children (mapcar
                                                 (lambda (x)
                                                   (cond ((typep x 'myfile) x)
                                                         ((typep x 'mydir) (if (equal (mydir-name x) (first p))
                                                                             (touch x (rest p) name size)
                                                                             x))))
                                                 (mydir-children d)))))

(defun myls (fs l)
  (when (null l) (return-from myls fs))
  (myls
    (make-fs
      :cwd (fs-cwd fs)
      :root (let ((s (split-stuff (first l))))
              (if (equal (first s) "dir")
                (mkdir (fs-root fs) (fs-cwd fs) (second s))
                (touch (fs-root fs) (fs-cwd fs) (second s) (parse-integer (first s))))))
    (rest l)))

(defun mycd (fs p)
  (make-fs
    :cwd (cond ((equal p "..") (reverse (rest (reverse (fs-cwd fs)))))
               ((equal p "/") nil)
               (t (append (fs-cwd fs) (list p))))
    :root (fs-root fs)))


(defun take-while (p l)
  (when (null l) (return-from take-while nil))
  (if (funcall p (first l))
    (cons (first l) (take-while p (rest l)))
    nil))

(defun drop-while (p l)
  (when (null l) (return-from drop-while nil))
  (if (funcall p (first l))
    (drop-while p (rest l))
    l))

(defun is-output (x) (not (equal (char x 0) #\$)))
(defun command-type (x) (cond ((string-equal (subseq x 0 2) "cd") 'cd)
                              ((string-equal (subseq x 0 2) "ls") 'ls)))

(defun get-command-as-function (c)
  (case (command-command-type c)
    ('cd (lambda (x)
           (mycd x (subseq (command-input c) 3))))
    ('ls (lambda (x)
           (myls x (command-output c))))))

(defun parse-command (l)
  (list
    (make-command
      :command-type (command-type (subseq (first l) 2))
      :input (subseq (first l) 2)
      :output (take-while #'is-output (rest l)))
    (drop-while #'is-output (rest l))))

(defun parse-commands (l)
  (when (null l) (return-from parse-commands nil))
  (let* ((c (parse-command l))
         (r (second c)))
    (cons (first c) (parse-commands r))))

(defun get-directory-size (d)
  (if (typep d 'myfile)
    (myfile-size d)
    (reduce '+ (mapcar #'get-directory-size (mydir-children d)))))

(defun get-all-dir-size (d)
  (if (typep d 'myfile)
    nil
    (cons (get-directory-size d) (reduce #'append (mapcar #'get-all-dir-size (mydir-children d))))))

(defun run-commands (l)
  (reduce (lambda (acc e)
            (funcall (get-command-as-function e) acc))
          l
          :initial-value (make-fs
                           :cwd nil
                           :root (make-mydir :name "/"))))

(defun part-1 (l)
  (reduce '+ (remove-if (lambda (x) (> x 100000))
                        (get-all-dir-size (fs-root (run-commands (parse-commands l)))))))

(defun part-2 (l)
  (let* ((root (fs-root (run-commands (parse-commands l))))
         (all-sizes (get-all-dir-size root))
         (used-space (get-directory-size root))
         (unused-space (- 70000000 used-space))
         (needed-space (- 30000000 unused-space)))
    (apply 'min (remove-if (lambda (x) (< x needed-space)) all-sizes))))

(format t "# PART 1 # Expected Result: ~a~%" (part-1 (get-file "./input.txt")))
(format t "# PART 2 # Expected Result: ~d~%" (part-2 (get-file "./input.txt")))
