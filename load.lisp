(declaim #+sbcl(sb-ext:muffle-conditions warning))

(load "closless.lisp")

; ./miniclos_implementation
(load "./miniclos_implementation/layer.lisp")
(load "./miniclos_implementation/classes.lisp")
(load "./miniclos_implementation/class-precedence-list.lisp")
(load "./miniclos_implementation/instances.lisp")
(load "./miniclos_implementation/functions_and_methods.lisp")
(load "./miniclos_implementation/method-calling.lisp")
(load "./miniclos_implementation/macros.lisp")