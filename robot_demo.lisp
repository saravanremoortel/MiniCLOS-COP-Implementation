(load "~/quicklisp/setup.lisp") ; if you want to use this, make sure quicklisp is istalled
(ql:quickload :contextl)
(in-package :contextl-user)

; --------------------------
; 1. Define Layers
; --------------------------
(deflayer night-mode)
(deflayer debug-mode)
(deflayer accessibility-mode)

; --------------------------
; 2. Define Base Classes
; --------------------------
(define-layered-class robot ()
  ((name :initarg :name :accessor robot-name)))

(define-layered-class service-robot (robot) ())
(define-layered-class security-robot (robot) ())

; --------------------------
; 3. Layered Functions
; --------------------------
(define-layered-function greet (r customer-type))

; --------------------------
; 4. Base Behavior
; --------------------------
(define-layered-method greet :in-layer t ((r service-robot) customer-type)
  (format t "~A says: Hello, ~A! Welcome to our store.~%"
          (robot-name r) customer-type))

(define-layered-method greet :in-layer t ((r security-robot) customer-type)
  (format t "~A says: Greetings, ~A. I am here to guarentee safety.~%"
          (robot-name r) customer-type))

; --------------------------
; 5. Night Mode Behavior
; --------------------------
(define-layered-method greet :in-layer night-mode ((r service-robot) customer-type)
  (format t "~A says: Good evening, ~A. We are closing soon.~%"
          (robot-name r) customer-type))

(define-layered-method greet :in-layer night-mode ((r security-robot) customer-type)
  (format t "~A says: Good evening, ~A. The premises are under security watch.~%"
          (robot-name r) customer-type))

; --------------------------
; 6. Accessibility Mode Behavior
; --------------------------
(define-layered-method greet :in-layer accessibility-mode :before ((r robot) customer-type)
  (format t "[Audio Alert] Accessibility assistance mode activated. Speaking clearly and providing detailed information.~%"))

(define-layered-method greet :in-layer accessibility-mode ((r service-robot) customer-type)
  (format t "~A says clearly: Hello ~A. I am your service assistant robot. I can provide audio directions to any department you need. How may I assist you today?~%"
          (robot-name r) customer-type))

(define-layered-method greet :in-layer accessibility-mode ((r security-robot) customer-type)
  (format t "~A says clearly: Welcome ~A. I am the security assistance robot. There is a ramp to your right for wheelchair access. Emergency exits are clearly marked with audio beacons. Please let me know if you need navigation assistance or have any accessibility requirements.~%"
          (robot-name r) customer-type))

(define-layered-method greet :in-layer accessibility-mode :after ((r robot) customer-type)
  (format t "[Audio Alert] Press the large blue button on my chest to repeat this information or request additional assistance.~%"))

; --------------------------
; 7. Debug Mode (Extra Info)
; --------------------------
(define-layered-method greet :in-layer debug-mode :after ((r robot) customer-type)
  (format t "[DEBUG] Robot '~A' greeted customer type '~A' at time ~A.~%"
          (robot-name r) customer-type (get-universal-time)))

; --------------------------
; 8. Runtime Context Switching
; --------------------------
(defun current-night-mode-active-p ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    ;(declare (ignore second minute date month year day-of-week dst-p tz)) :good practice, cuz some bound variables are unused, it avoids warnings
    (or (< hour 8) (> hour 18))))  ; before 8 AM or after 6 PM

(defun greet-customer (robot customer-type accessibility-needed-p)
  (cond
    (accessibility-needed-p
     (with-active-layers (accessibility-mode)
       (if (current-night-mode-active-p)
           (with-active-layers (night-mode)
             (greet robot customer-type))
           (greet robot customer-type))))
    ((current-night-mode-active-p)
     (with-active-layers (night-mode)
       (greet robot customer-type)))
    (t
     (greet robot customer-type))))

; --------------------------
; 9. Demo Run
; --------------------------
(defun run-robot-demo ()
  (let ((service-bot (make-instance 'service-robot :name "Service-bot-1"))
        (security-bot (make-instance 'security-robot :name "Security-bot-1")))

    (format t "~%--- Normal Daytime Greeting ---~%")
    (greet-customer service-bot "guest" nil)
    (greet-customer security-bot "guest" nil)

    (format t "~%--- Night Mode Greeting ---~%")
    (with-active-layers (night-mode)
      (greet service-bot "guest")
      (greet security-bot "guest"))

    (format t "~%--- Accessibility Mode Greeting ---~%")
    (greet-customer service-bot "customer" t)
    (greet-customer security-bot "visitor" t)

    (format t "~%--- Accessibility + Debug Mode ---~%")
    (with-active-layers (accessibility-mode debug-mode)
      (greet service-bot "customer")
      (greet security-bot "visitor"))))

; Run demo
(run-robot-demo)

