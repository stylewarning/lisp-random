;;; (ql:quickload :lispbuilder-sdl)
;;; Loading ("C:/Windows/SysWOW64/SDL.dll") on Windows.

(defun lemniscate (&key (center (sdl:point :x 0 :y 0))
                        (focal-distance  20))
  (lambda (θ)
    (let* ((cos-θ (cos θ))
           (sin-θ (sin θ))
           (denom (1+ (* sin-θ sin-θ)))
           (scale (* focal-distance (sqrt 2) cos-θ))
           (x     (floor scale denom))
           (y     (floor (* scale sin-θ) denom)))
      (sdl:point :x (+ x (sdl:x center))
                 :y (+ y (sdl:y center))))))

(defun parametric-plot (f τ-min τ-max &optional (τ-step 1))
  (loop :for τ :from τ-min :to τ-max :by τ-step
        :do (sdl:draw-pixel (funcall f τ))))

(defun run ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:draw-rectangle-* 50 50 60 40)
    (sdl:draw-string-solid-* "hi #symbollox" 180 150
                             :font (sdl:initialise-font sdl:*font-10x20*)
                             :color sdl:*cyan*)
    (parametric-plot (lemniscate :center (sdl:point :x 100 :y 160))
                     0 (* 2 pi) 0.01)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))