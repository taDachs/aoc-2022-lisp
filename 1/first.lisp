#!/usr/bin/clisp

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-when (pred l &optional acc)
  (when (null l) (return-from split-when (list (reverse acc))))
  (if (funcall pred (car l))
    (cons (reverse acc) (split-when pred (cdr l)))
    (split-when pred (cdr l) (cons (car l) acc))))

(defun get-weight-per-elf (l)
  (mapcar (lambda (x) (apply '+ (mapcar #'parse-integer x))) (split-when (lambda (x) (= (length x) 0)) l)))

(defun get-max-elf-weight (l)
  (apply #'max (get-weight-per-elf l)))

(defun merge-sort (l)
  (labels ((quick-merge (a b)
                        (when (null a) (return-from quick-merge b))
                        (when (null b) (return-from quick-merge a))
                        (if (< (car a) (car b))
                          (cons (car a) (quick-merge (cdr a) b))
                          (cons (car b) (quick-merge a (cdr b)))))
           (quick-split (l)
                        (list
                          (subseq l 0 (floor (length l) 2))
                          (subseq l (floor (length l) 2)))))
    (if (<= (length l) 1)
      l
      (let ((splits (quick-split l)))
        (quick-merge (merge-sort (first splits)) (merge-sort (second splits)))))))


(defun get-top-tree-elf-weights (l)
  (apply '+ (subseq (reverse (merge-sort (get-weight-per-elf l))) 0 3)))


(print (get-max-elf-weight (get-file "./input.txt")))
(print (get-top-tree-elf-weights (get-file "./input.txt")))
