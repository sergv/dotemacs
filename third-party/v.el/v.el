;; v.el --- -*- lexical-binding: t; -*-

;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 21 February 2020

;; License:
;; Copyright 2020 Sergey Vinokurov
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Description:
;; A modern library for manipulating vectors.

(defmacro v--find (predicate xs)
  "Anaphoric form of `v-find'."
  (let ((res-var '#:res)
        (in-var '#:in)
        (len-var '#:len)
        (i-var '#:i)
        (elem-var '#:elem))
    `(let* ((,in-var ,xs)
            (,i-var 0)
            (,len-var (length ,in-var))
            (,res-var nil))
       (while (and (< ,i-var ,len-var)
                   (not ,res-var))
         (let ((,elem-var (aref ,in-var ,i-var)))
           (when (let ((it ,elem-var)) ,predicate)
             (setf ,res-var ,elem-var)))
         (setf ,i-var (1+ ,i-var)))
       ,res-var)))

(defun v-find (f xs)
  "Find first element in vector XS where (f x) returns non-nil."
  (v--find (funcall f it) xs))

(defmacro v--map (f xs)
  "Anaphoric form of `v-map'."
  (let ((out-var '#:out)
        (in-var '#:in)
        (len-var '#:len)
        (i-var '#:i))
    `(let* ((,in-var ,xs)
            (,len-var (length ,in-var))
            (,out-var (make-vector ,len-var nil)))
       (dotimes (,i-var ,len-var)
         (aset ,out-var ,i-var (let ((it (aref ,in-var ,i-var))) ,f)))
       ,out-var)))

(defun v-map (f xs)
  "Apply function F to all elements of a vector XS."
  (v--map (funcall f it) xs))

(defun v-assq (key xs)
  "Return non-nil if KEY is `eq' to the car of an element of vector XS.
Return whole cons cell, if one's present.

NOTE: while `assq' will ignore non-cons-cells in collection, this function
will raise an error on such."
  (v--find (eq key (car it)) xs))

(defun v-member (key xs)
  "Return t if KEY is an element of XS  Comparison done with ‘equal’."
  (v--find (equal key it) xs))

(provide 'v)

;; Local Variables:
;; End:

;; v.el ends here
