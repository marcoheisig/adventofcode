(defsystem :adventofcode
  :description "Infrastructure for solving 'Advent of Code' exercises."
  :author "Marco Heisig <marco.heisig@fau.de>"

  :depends-on
  ("alexandria"
   "split-sequence"
   "closer-mop"
   "cl-ppcre"
   "trivia")

  :serial t
  :components
  ((:module "utilities"
    :components
    ((:file "packages")
     (:file "initialize")))))
