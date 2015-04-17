;;;; GIF decoder for LispWorks
;;;;
;;;; Copyright (c) 2015 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :gif)

(defclass gif-animation ()
  ((gif    :initarg :gif    :reader gif-animation-gif)
   (frames :initarg :frames :reader gif-animation-frames))
  (:documentation "A series of images that make up a GIF animation."))

(defclass gif-animation-frame ()
  ((image  :initarg :image  :reader gif-animation-frame-image)
   (delay  :initarg :delay  :reader gif-animation-frame-delay))
  (:documentation "A single image frame in a GIF animation."))

(defun animated-gif-p (gif)
  "Checks for the NETSCAPE application extension in the GIF."
  (loop for ext across (gif-extensions gif)
        when (and (typep ext 'application-extension) (string-equal (application-extension-id ext) "NETSCAPE"))
        return t))

(defun make-gif-animation (port gif)
  "Creates a series of images for a single GIF animation."
  (let ((frames (loop with image = nil
                      
                      ;; coalesce all the images together
                      for data-block across (gif-image-data-blocks gif)
                      for new-image = (make-gif-image port gif data-block image)

                      ;; get the delay for this image from the graphics control extension (if present)
                      for delay = (let ((ext (image-data-block-extension data-block)))
                                    (if (null ext)
                                        1
                                      (graphic-control-extension-delay-time ext)))
                      
                      ;; update the image
                      collect (prog1 (make-instance 'gif-animation-frame :image new-image :delay delay)
                                (setf image new-image)))))

    ;; create the combined animation object
    (make-instance 'gif-animation :gif gif :frames frames)))

(defun play-gif-animation (pane anim x y &key repeat-count)
  "Spawns a process to play an animation in a graphics port."
  (flet ((play ()
           (loop for count from 0 until (eql count repeat-count)

                 ;; play each frame
                 do (loop for frame in (gif-animation-frames anim)

                          ;; get the image and delay
                          for image = (gif-animation-frame-image frame)
                          for delay = (gif-animation-frame-delay frame)

                          ;; render it
                          do (progn
                               (apply-in-pane-process-if-alive pane #'gp:draw-image pane image x y)

                               ;; wait before showing the next frame in the animation
                               (mp:current-process-pause delay))))))

    ;; start the process going
    (mp:process-run-function "GIF animation" () #'play)))
