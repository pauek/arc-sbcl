
(defsystem arc
  :description "Arc implementation"

  :components
  ((:file "pkgs")
   (:file "util")
   (:file "lex")
   (:file "ac")
   (:file "prim"))
  :serial t

  :depends-on (:pk-walk :sb-bsd-sockets)

  :perform
  (test-op :after (op c)
    (operate 'load-op :arc/test :verbose nil)
    (operate 'test-op :arc/test :force t)))

(defsystem arc/test
  :components
  ((:module "test"
    :pathname "test/"
    :components
    ((:file "pkgs")
     (:file "util")
     (:file "ac")
     (:file "mac")
     (:file "prim"))
    :serial t))

  :depends-on (:arc)

  :perform
  (test-op :after (op c)
    (let ((*package* (find-package :arc/test)))
      (funcall (intern (symbol-name ':run))))))
