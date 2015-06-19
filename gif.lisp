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

(defpackage :gif
  (:use :cl :lw :capi :bit-stream)
  (:export
   #:read-gif
   #:load-gif

   ;; images and animation
   #:make-gif-image
   #:make-gif-animation

   ;; gif objects
   #:gif-version
   #:gif-logical-screen-descriptor
   #:gif-extensions
   #:gif-image-data-blocks

   ;; gif screen descriptor
   #:logical-screen-descriptor-width
   #:logical-screen-descriptor-height
   #:logical-screen-descriptor-resolution
   #:logical-screen-descriptor-background-color
   #:logical-screen-descriptor-aspect-ratio
   #:logical-screen-descriptor-color-table
   #:logical-screen-descriptor-color-table-sorted-p

   ;; gif application extension
   #:application-extension-id
   #:application-extension-auth-code
   #:application-extension-data

   ;; gif comment extension
   #:comment-extension-text

   ;; gif graphic control extension
   #:graphic-control-extension-disposal-method
   #:graphic-control-extension-delay-time
   #:graphic-control-extension-user-p
   #:graphic-control-extension-transparent-p
   #:graphic-control-extension-transparent-color

   ;; gif plain text extension
   #:plain-text-extension-text-grid-left-pos
   #:plain-text-extension-text-grid-top-pos
   #:plain-text-extension-text-grid-width
   #:plain-text-extension-text-grid-height
   #:plain-text-extension-cell-width
   #:plain-text-extension-cell-height
   #:plain-text-extension-foreground-color
   #:plain-text-extension-background-color
   #:plain-text-extension-text

   ;; gif animations
   #:gif-animation-gif
   #:gif-animation-frames
   
   ;; gif animation frames
   #:gif-animation-frame-image
   #:gif-animation-frame-delay))

(in-package :gif)

(defclass gif ()
  ((version    :initarg :version            :reader gif-version)
   (screen     :initarg :screen             :reader gif-logical-screen-descriptor)
   (extensions :initarg :extensions         :reader gif-extensions)
   (images     :initarg :images             :reader gif-image-data-blocks))
  (:documentation "Representation of a GIF in memory."))

(defclass logical-screen-descriptor ()
  ((width      :initarg :width              :reader logical-screen-descriptor-width)
   (height     :initarg :height             :reader logical-screen-descriptor-height)
   (res        :initarg :resolution         :reader logical-screen-descriptor-resolution)
   (background :initarg :background-color   :reader logical-screen-descriptor-background-color)
   (aspect     :initarg :aspect-ratio       :reader logical-screen-descriptor-aspect-ratio)
   (colors     :initarg :color-table        :reader logical-screen-descriptor-color-table)
   (sort       :initarg :sorted-p           :reader logical-screen-descriptor-color-table-sorted-p))
  (:documentation "GIF size in pixels, resolution, global color table, etc."))

(defclass application-extension ()
  ((id         :initarg :id                 :reader application-extension-id)
   (auth-code  :initarg :auth-code          :reader application-extension-auth-code)
   (data       :initarg :data               :reader application-extension-data))
  (:documentation "Application-specific extensions."))

(defclass comment-extension ()
  ((text       :initarg :text               :reader comment-extension-text))
  (:documentation "Random comments inside a GIF (e.g. copyright)."))

(defclass graphic-control-extension ()
  ((disposal   :initarg :disposal-method    :reader graphic-control-extension-disposal-method)
   (delay      :initarg :delay-time         :reader graphic-control-extension-delay-time)
   (user-p     :initarg :user-p             :reader graphic-control-extension-user-p)
   (trans-p    :initarg :transparent-p      :reader graphic-control-extension-transparent-p)
   (trans-idx  :initarg :transparent-color  :reader graphic-control-extension-transparent-color))
  (:documentation "Information about how an image-data-block is meant to be rendered."))

(defclass plain-text-extension ()
  ((left       :initarg :text-grid-left-pos :reader plain-text-extension-text-grid-left-pos)
   (top        :initarg :text-grid-top-pos  :reader plain-text-extension-text-grid-top-pos)
   (width      :initarg :text-grid-width    :reader plain-text-extension-text-grid-width)
   (height     :initarg :text-grid-height   :reader plain-text-extension-text-grid-height)
   (cell-w     :initarg :cell-width         :reader plain-text-extension-cell-width)
   (cell-h     :initarg :cell-height        :reader plain-text-extension-cell-height)
   (foreground :initarg :foreground-color   :reader plain-text-extension-foreground-color)
   (background :initarg :background-color   :reader plain-text-extension-background-color)
   (text       :initarg :text               :reader plain-text-extension-text))
  (:documentation "Render plain text on top of a GIF after it has been rendered."))

(defclass image-data-block ()
  ((extension  :initarg :extension          :reader image-data-block-extension)
   (left       :initarg :left               :reader image-data-block-left)
   (top        :initarg :top                :reader image-data-block-top)
   (width      :initarg :width              :reader image-data-block-width)
   (height     :initarg :height             :reader image-data-block-height)
   (interlaced :initarg :interlaced-p       :reader image-data-block-interlaced-p)
   (colors     :initarg :color-table        :reader image-data-block-color-table)
   (sorted-p   :initarg :sorted-p           :reader image-data-block-color-table-sorted-p)
   (indices    :initarg :index-stream       :reader image-data-block-index-stream))
  (:documentation "A single frame. Might be a portion of - or the entire - screen."))

(defun test ()
  (load-gif #p"~/.lispworks.d/gif/test/interlaced.gif"))

(defun read-short (stream)
  "Returns an unsigned, 16-bit fixnum in big-endian format from the stream."
  (logior (read-byte stream) (ash (read-byte stream) 8)))

(defun read-long (stream)
  "Returns an unsigned, 32-bit fixnum in big-endian format from the stream."
  (logior (read-short stream) (ash (read-short stream) 16)))

(defun read-color-table (stream size)
  "Returns an array of all the colors in the GIF color table."
  (loop with color-table = (make-array size)

        ;; loop over the entire color table
        for i below size

        ;; read in each octet
        for red = (/ (read-byte stream) 255)
        for green = (/ (read-byte stream) 255)
        for blue = (/ (read-byte stream) 255)

        ;; fill in the color table
        do (setf (aref color-table i) (color:make-rgb red green blue))

        ;; return it
        finally (return color-table)))

(defun read-data-block (stream size &key string-p)
  "Returns an array of bytes that make up a data block. Optionally coerce to a string."
  (let ((data-block (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence data-block stream)

    ;; optionally coerce the data block to a string
    (if (null string-p)
        data-block
      (map 'string #'code-char data-block))))

(defun read-data-blocks (stream &key string-p)
  "Return an array of data blocks, optionally coerced to a string."
  (loop for size = (read-byte stream)

        ;; a zero-length block terminates the list of blocks
        until (zerop size)

        ;; read the next block of data
        collect (read-data-block stream size :string-p string-p)))

(defun read-header (stream)
  "T if the stream begins with GIF. Return the version # or nil."
  (when (and (eql (read-byte stream nil nil) #x47)
             (eql (read-byte stream nil nil) #x49)
             (eql (read-byte stream nil nil) #x46))
    (map 'string #'code-char (list (read-byte stream)
                                   (read-byte stream)
                                   (read-byte stream)))))

(defun read-logical-screen (stream)
  "Returns the width and height of a GIF."
  (let* ((width (read-short stream))
         (height (read-short stream))

         ;; fields that will be expanded
         (packed (read-byte stream))

         ;; resolution of the GIF
         (res (1+ (logand (ash packed -4) #x07)))

         ;; T if there is a global color table
         (gct-p (plusp (logand packed #x80)))

         ;; T if the color table is sorted (if present)
         (sorted-p (plusp (logand packed #x08)))

         ;; number of entrie in the color table (if present)
         (gct-size (ash 1 (1+ (logand packed #x07))))

         ;; the background color index
         (bg (read-byte stream))
         
         ;; aspect ratio
         (ar (let ((byte (read-byte stream)))
               (if (zerop byte)
                   nil
                 (/ (+ byte 15) 64))))

         ;; read the color table if there is one
         (color-table (and gct-p (read-color-table stream gct-size))))

    ;; create the logical screen descriptor
    (make-instance 'logical-screen-descriptor
                   :width width
                   :height height
                   :resolution res
                   :background-color bg
                   :aspect-ratio ar
                   :color-table color-table
                   :sorted-p sorted-p)))

(defun read-comment-extension (stream)
  "Appends all data blocks into a single comment text."
  (let ((blocks (read-data-blocks stream :string-p t)))
    (make-instance 'comment-extension :text (format nil "~{~a~^ ~}" blocks))))

(defun read-plain-text-extension (stream)
  "Return a plain text GIF extension block."
  (let ((size (read-byte stream))
        
        ;; top/left position
        (text-grid-left-pos (read-short stream))
        (text-grid-top-pos (read-short stream))
        
        ;; size of grid
        (text-grid-width (read-short stream))
        (text-grid-height (read-short stream))
        
        ;; cell size
        (cell-width (read-byte stream))
        (cell-height (read-byte stream))
        
        ;; colors
        (fg-color-index (read-byte stream))
        (bg-color-index (read-byte stream))

        ;; sub-blocks of text
        (text (read-data-blocks stream :string-p t)))

    ;; the block size should always be 12
    (assert (eql size 12))

    ;; create the extension and return it
    (make-instance 'plain-text-extension
                   :text-grid-left-pos text-grid-left-pos
                   :text-grid-top-pos text-grid-top-pos
                   :text-grid-width text-grid-width
                   :text-grid-height text-grid-height
                   :cell-width cell-width
                   :cell-height cell-height
                   :foreground-color fg-color-index
                   :background-color bg-color-index
                   :text text)))

(defun read-application-extension (stream)
  "Read an application extension block. Usually not used."
  (let ((size (read-byte stream))

        ;; read the application identifier and authentication code
        (app-id (make-array 8 :element-type '(unsigned-byte 8)))
        (app-auth-code (make-array 3 :element-type '(unsigned-byte 8))))
    (read-sequence app-id stream)
    (read-sequence app-auth-code stream)

    ;; all application blocks are 11 bytes
    (assert (eql size 11))

    ;; read the sub-blocks for the application data
    (make-instance 'application-extension
                   :id (map 'string #'code-char app-id)
                   :auth-code (map 'string #'code-char app-auth-code)
                   :data (read-data-blocks stream))))

(defun read-graphic-control-extension (stream)
  "Read a GIF extension block."
  (let* ((size (read-byte stream))

         ;; packed fields
         (fields (read-byte stream))
         
         ;; disposal method from packed field
         (disposal-method (case (logand fields #x1c)
                            (#x04 :do-not-dispose)
                            (#x08 :restore-to-backgorund)
                            (#x0c :restore-to-previous)))
         
         ;; packed field flags
         (user-p (plusp (logand fields #x02)))
         (transparency-p (plusp (logand fields #x01)))

         ;; delay time (in 1/100ths of a second)
         (delay (/ (read-short stream) 100))

         ;; color index
         (transparent-index (read-byte stream)))

    ;; must be 4 bytes in size
    (assert (eql size 4))

    ;; must end with a block terminator
    (assert (zerop (read-byte stream)))

    ;; create the extension
    (make-instance 'graphic-control-extension
                   :disposal-method disposal-method
                   :delay-time delay
                   :user-p user-p
                   :transparent-p transparency-p
                   :transparent-color transparent-index)))

(defun read-extension-block (stream)
  "Read an extension."
  (let ((extension-type (read-byte stream)))
    (case extension-type
      (#x01 (read-plain-text-extension stream))
      (#xf9 (read-graphic-control-extension stream))
      (#xfe (read-comment-extension stream))
      (#xff (read-application-extension stream))

      ;; unknown extension, warn, read the block, be done
      (otherwise (warn "Unknown extension: #x~16r... skipping.~%" extension-type)
                 (let ((size (read-byte stream)))
                   (file-position stream (+ (file-position stream) size)))))))

(defun read-lzw-blocks (stream)
  "Read a set of compressed, image data blocks."
  (loop with bytes = (make-array 128 :adjustable t :fill-pointer 0)
        
        ;; loop over all the blocks
        for block-size = (read-byte stream)
        
        ;; stop when there's a zero-length block
        do (if (zerop block-size)
               (return (make-input-bit-stream bytes))
             (dotimes (i block-size)
               (vector-push-extend (read-byte stream) bytes)))))

(defun read-image-data (color-table stream)
  "Read the bytes (and decode) a GIF image."
  (loop with image = (make-array 0 :adjustable t :fill-pointer t)
        with min-code-size = (1+ (read-byte stream))
        with code-size = min-code-size
        with code-stream = (read-lzw-blocks stream)
        with clear-code = (stream-read-bits code-stream code-size)
        with stop-code = (1+ clear-code)
        with code-1 = nil
        
        ;; create an initial code table
        with code-table = (loop with size = (+ (length color-table) 2)
                                with codes = (make-array size :adjustable t :fill-pointer t)
                                for i below (length color-table)
                                do (setf (aref codes i) (list i))
                                finally (return codes))
        
        ;; read codes
        for code = (stream-read-bits code-stream code-size)
        
        ;; stop at the terminal
        do (cond ((= code stop-code)
                  (return image))
                 
                 ;; reset at a clear code
                 ((= code clear-code)
                  (setf code-1 nil
                        code-size min-code-size)
                  
                  ;; reset the code table back to the reset position
                  (setf (fill-pointer code-table) (+ (length color-table) 2)))
                 
                 ;; the code is already in the code table
                 ((< code (length code-table))
                  (let ((seq (aref code-table code)))
                    (dolist (i seq)
                      (vector-push-extend i image))
                    (when code-1
                      (vector-push-extend (append code-1 (list (first seq))) code-table))))
                 
                 ;; the code is new and needs to be added to the table
                 (t (let ((seq (append code-1 (list (first code-1)))))
                      (dolist (i seq)
                        (vector-push-extend i image))
                      (vector-push-extend seq code-table))))
        
        ;; do we need to increase the code-size?
        when (and (< code-size 12) (= (length code-table) (ash 1 code-size)))
        do (incf code-size)
        
        ;; update the previous code to this code
        do (setf code-1 (aref code-table code))))

(defun read-image-descriptor (stream &key gct ext)
  "Read an image descriptor."
  (let* ((left (read-short stream))
         (top (read-short stream))
         (width (read-short stream))
         (height (read-short stream))
         
         ;; packed byte for bit fields
         (packed (read-byte stream))

         ;; local color table?
         (lct-p (plusp (logand packed #x80)))

         ;; interlaced?
         (interlaced-p (plusp (logand packed #x40)))

         ;; sorted color table?
         (sorted-p (plusp (logand packed #x20)))

         ;; local color table size
         (lct-size (ash 1 (1+ (logand packed #x07))))

         ;; read the local color table if there is one
         (color-table (if (null lct-p)
                          gct
                        (read-color-table stream lct-size)))

         ;; read the image data
         (frame (read-image-data color-table stream)))

    ;; create the image
    (make-instance 'image-data-block
                   :extension ext
                   :left left
                   :top top
                   :width width
                   :height height
                   :interlaced-p interlaced-p
                   :color-table color-table
                   :sorted-p sorted-p
                   :index-stream frame)))

(defun read-gif (stream)
  "Read an entire GIF from a stream of bytes. Return a GIF structure."
  (when-let (version (read-header stream))

    ;; read the logical screen descriptor and data
    (let ((screen-descriptor (read-logical-screen stream)))
      
      ;; read all the data for the image
      (loop with gct = (logical-screen-descriptor-color-table screen-descriptor)
              
            ;; allocate extensions and image buffers
            with extensions = (make-array 0 :adjustable t :fill-pointer t)
            with images = (make-array 0 :adjustable t :fill-pointer t)
              
            ;; read each section of the GIF
            for c = (read-byte stream)
            
            ;; stop at the GIF terminator byte
            when (eql c #x3b)
            return (make-instance 'gif
                                  :version version
                                  :screen screen-descriptor
                                  :extensions extensions
                                  :images images)
              
            ;; check for an extension or graphic block
            do (case c
                 (#x21 (let ((ext (read-extension-block stream)))
                         (etypecase ext
                           (application-extension (vector-push-extend ext extensions))
                           (plain-text-extension (vector-push-extend ext extensions))
                           (comment-extension (vector-push-extend ext extensions))
                             
                           ;; graphics control extensions wrap frames
                           (graphic-control-extension
                            (case (read-byte stream)
                              (#x21 (read-extension-block stream))
                                
                              ;; image data after the graphics control extension
                              (#x2c (let ((image (read-image-descriptor stream :gct gct :ext ext)))
                                      (vector-push-extend image images))))))))
                   
                 ;; image block (with no graphics control extension
                 (#x2c (let ((image (read-image-descriptor stream :gct gct)))
                         (vector-push-extend image images)))
                   
                 ;; unknown block
                 (otherwise (let ((size (read-byte stream)))
                              (read-data-block stream size))
                            (warn "Unknown GIF block type #x~x... skipping.~%" c)))))))

(defun load-gif (pathname)
  "Load a GIF structure from a file on disk."
  (with-input-bit-stream (stream pathname)
    (read-gif stream)))

(defun make-gif-image (port gif &optional data-block overlay merge-p)
  "Create a new graphics port image from a GIF data block, optionally overlayed on another image."
  (let* ((w (logical-screen-descriptor-width (gif-logical-screen-descriptor gif)))
         (h (logical-screen-descriptor-height (gif-logical-screen-descriptor gif)))

         ;; merge onto the overlay, start from the overlay, or create a new image
         (image (if overlay
                    (if merge-p
                        overlay
                      (gp:make-sub-image port overlay))
                  (gp:make-image port w h :alpha t))))

    (flet ((render-data-block (data-block)
             (let ((left (image-data-block-left data-block))
                   (top (image-data-block-top data-block))
                   (width (image-data-block-width data-block))
                   (height (image-data-block-height data-block))
         
                   ;; is this data block interlaced?
                   (interlaced-p (image-data-block-interlaced-p data-block))
         
                   ;; get the transparency index (if there is one)
                   (transparency-index (when-let (ext (image-data-block-extension data-block))
                                         (when (graphic-control-extension-transparent-p ext)
                                           (graphic-control-extension-transparent-color ext))))
         
                   ;; get the color table to use
                   (color-table (let ((table (image-data-block-color-table data-block)))
                                  (map 'vector #'(lambda (c) (color:convert-color port c)) table)))
         
                   ;; the data image block of color indices
                   (index-stream (image-data-block-index-stream data-block))
         
                   ;; create an image access object
                   (access (gp:make-image-access port image)))
    
               ;; write the image data
               (unwind-protect
                   (flet ((render-line (from-y to-y)
                            (dotimes (x width)
                              (when-let (c (let ((i (aref index-stream (+ (* from-y width) x))))
                                             (unless (eql i transparency-index)
                                               (aref color-table i))))
                                (setf (gp:image-access-pixel access (+ x left) (+ to-y top)) c)))))
                     
                     ;; if the image is interlaced then the scanlines are out of order
                     (if interlaced-p
                         (loop with from-y = 0
                               for (start step) in '((0 8) (4 8) (2 4) (1 2))
                               do (loop for to-y from start below height by step
                                        do (progn
                                             (render-line from-y to-y)
                                             (incf from-y)))
                               finally (return image))
                       (loop for from-y below height do (render-line from-y from-y))))
                 (gp:free-image-access access)))))

      ;; TODO: if there's no overlay, write the background color first

      ;; if there's no data-block selected, render them all, return the image
      (prog1 image
        (if (null data-block)
            (loop for i across (gif-image-data-blocks gif) do (render-data-block i))
          (render-data-block data-block))))))