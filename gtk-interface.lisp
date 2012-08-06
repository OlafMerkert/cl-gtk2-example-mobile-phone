(defpackage :mobile-phone-gtk-interface
  (:shadowing-import-from :gtk :range)
  (:nicknames :mobi-gtk)
  (:use :cl :ol
        :gtk :gdk :gobject)
  (:export))

(in-package :mobile-phone-gtk-interface)

(defclass line-display ()
  ((nr-of-lines    :initarg :nr-of-lines
                   :initform 4
                   :reader nr-of-lines)
   (chars-per-line :initarg :chars-per-line
                   :initform 20
                   :reader chars-per-line))
  (:documentation "doc"))

(defgeneric text (display))
(defgeneric set-text (display value))

(defsetf text set-text)

(defgeneric line (display line-nr))
(defgeneric set-line (display line-nr value))

(defsetf line set-line)

(defclass line-display/text-view (line-display)
  ((text-buffer :initarg :text-buffer
                :reader text-buffer))
  (:documentation "doc"))

(defmethod initialize-instance :after ((line-display line-display/text-view) &key)
  (setf (text-buffer-text (text-buffer line-display))
        (with-output-to-string (stream)
          (dotimes (i (nr-of-lines line-display))
            (dotimes (j (chars-per-line line-display))
              (princ #\. stream))
            (terpri stream)))))

(defmethod text ((line-display line-display/text-view))
  (text-buffer-text (text-buffer line-display)))

(defmethod set-text ((line-display line-display/text-view) (value string))
  ;; todo ensure we have exactly the correct number of lines and chars per line
  (setf (text-buffer-text (text-buffer line-display))
        value))

(defmethod line ((line-display line-display) (line-nr integer))
  ;; account for line breaks
  (subseq (text line-display)
          (* line-nr
             (+ (chars-per-line line-display) 1))
          (- (* (+ line-nr 1)
                (+ (chars-per-line line-display) 1))
             1))
  ;; todo check for line count and sufficiently filled text-buffer
  )

(defun ->string (object)
  (format nil "~A" object))

(defparameter buttons
  '((pickup (next prev) hangup)
    (1 2 3)
    (4 5 6)
    (7 8 9)
    (* 0 \#))
  "specify a table of buttons/vertically stacked buttons")

(defun create-button-interface (button-click-function)
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "Mobile Phone Emulator"
             :default-heigth 800
             :default-width 300
             :var window
             (v-box
              ;; todo use larger font size and monospace font
              (text-view :var display-widget
                         :editable nil
                         :wrap-mode :none
                         :justification :left
                         :buffer (make-instance 'text-buffer :text ""))
              (table :homogeneous t
                     :n-columns 3
                     :n-rows (length buttons)
                     :var button-table)))
      ;; on closing
      (connect-signal window "destroy"
                      (ilambda (w) (leave-gtk-main)))
      ;; put the buttons in the interface
      (let ((display (make-instance 'line-display/text-view
                                    :text-buffer (text-view-buffer display-widget))))
       (loop
          for i from 0
          and button-row in buttons
          do (loop
                for j from 0
                and button-label in button-row
                do (if (listp button-label)
                       ;; if a button-spec is a list, stack the buttons
                       ;; in there vertically
                       (let* ((button-labels button-label)
                              (buttons (mapcar
                                        (lambda (button-label)
                                          (make-instance
                                           'button
                                           :label (->string button-label)))
                                        button-labels))
                              (bbox (make-instance 'v-button-box)))
                         (loop
                            for button in buttons
                            and button-label in button-labels
                            do
                              (box-pack-start bbox button)
                              (connect-signal button "clicked"
                                              ;; a new let to close over
                                              #1=(let ((button-identifier button-label)) 
                                                   (ilambda (w)
                                                     (funcall button-click-function
                                                              display button-identifier)))))
                         (table-attach button-table bbox
                                       j (+ j 1)
                                       i (+ i 1)))
                       ;; otherwise just put the one button in the grid.
                       (let ((button (make-instance
                                      'button
                                      :label (->string button-label))))
                         (table-attach button-table button
                                       j (+ j 1)
                                       i (+ i 1))
                         (connect-signal button "clicked"
                                         #1#))))))
      ;; show the window
      (widget-show window))))

(defun dummy-button-hander (display button-symbol)
  (setf (text-buffer-text (text-buffer display))
        (format nil "Clicked button: ~A~%" button-symbol)))
