(defun solve (n)
  (loop
    :for x :from 1 :below n
    :when (or (zerop (mod x 3)) (zerop (mod x 5)))
    :sum x))

(assert (= (solve 10) 2))
(assert (= (solve 1000) 233168))
