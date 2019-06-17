(defstruct (node(:constructor make-node(prev val next))) val next prev)

(defclass dll()
  ((head 
     :initform (make-node nil nil nil)
     :accessor get-head)))

(defmethod insert((obj dll) val)
  (defvar tmp)
  (setf tmp (get-head obj))
  (if (not (node-val tmp))
    (setf (node-val (get-head obj )) val)
    (setf (node-next (loop
                       (when (not (node-next tmp)) (return tmp))
                         (setf tmp (node-next tmp)))) 
          (make-node tmp val nil))))
    

(defmethod take-out((obj dll) val)
  ;; for some reason initializing on the same line with defvar is causing a bug
  ;;(defvar tmp (get-head obj))
  ;;(print (node-val tmp))
  ;; this is not causing a bug
  (defvar tmp)
  (setf tmp (get-head obj))
  ;; case 1 node at the head
  (if (and (not (node-next tmp)) (= (node-val tmp) val)) 
    (setf (node-val tmp) nil)
    (if (= (node-val tmp) val)
      (progn
        (setf (get-head obj) (node-next tmp))
        (setf (node-prev tmp) nil)
        )
      (loop
        (when (and (not (node-next tmp)) (not (= (node-val tmp) val))) (return nil)) ;; nothing to remove
        (if (and (= (node-val (node-next tmp)) val) (not (node-next (node-next tmp))))
          (progn
            (setf (node-next tmp) nil)
            (return tmp))
          (if (= (node-val (node-next tmp)) val)
            (progn
              (setf (node-prev (node-next (node-next tmp))) tmp)
              (setf (node-next tmp) (node-next (node-next tmp)))
              (return tmp))))
        (setf tmp (node-next tmp))))))

(defmethod print-dll((obj dll))
  (defvar tmp)
  (setq tmp (get-head obj))
  (if (node-val tmp)
    (loop 
        (when (not tmp) (return-from print-dll tmp))
        (print (node-val tmp))
        (setq tmp (node-next tmp)))
    (return-from print-dll tmp)))

(defvar obj)
(setq obj (make-instance 'dll))

;;;(print (insert obj 1))
;;;(print (take-out obj 2))


;;;(print (node-val (make-node 2)))
;;;(print-dll obj)


(insert obj 1)
(insert obj 2)
(insert obj 3)
(take-out obj 2)
(print-dll obj)
