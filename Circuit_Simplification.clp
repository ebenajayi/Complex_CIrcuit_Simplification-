(deftemplate MAIN::R
    (slot Resistor)
    (slot Node_1)
    (slot Node_2 )
    (slot Res_value))

;;Assign values to each resistor
(deffacts circuit1
    (R (Resistor R1) (Node_1 A) (Node_2 B) (Res_value 2))
    (R (Resistor R2) (Node_1 B) (Node_2 C) (Res_value 2))
    (R (Resistor R3) (Node_1 B) (Node_2 D) (Res_value 4))
    (R (Resistor R4) (Node_1 C) (Node_2 D) (Res_value 3))
    (R (Resistor R5) (Node_1 C) (Node_2 E) (Res_value 4))
    (R (Resistor R6) (Node_1 D) (Node_2 E) (Res_value 2)))

(deffacts circuit2
(R (Resistor R1) (Node_1 A) (Node_2 B) (Res_value 2))
(R (Resistor R2) (Node_1 B) (Node_2 E) (Res_value 5))
(R (Resistor R3) (Node_1 B) (Node_2 D) (Res_value 1))
(R (Resistor R4) (Node_1 B) (Node_2 C) (Res_value 4))
(R (Resistor R5) (Node_1 C) (Node_2 D) (Res_value 4))
(R (Resistor R6) (Node_1 D) (Node_2 E) (Res_value 3))
(R (Resistor R7) (Node_1 A) (Node_2 C) (Res_value 2))
(R (Resistor R8) (Node_1 I) (Node_2 C) (Res_value 4))
(R (Resistor R9) (Node_1 L) (Node_2 H) (Res_value 3))
(R (Resistor R10) (Node_1 H) (Node_2 I) (Res_value 1))
(R (Resistor R11) (Node_1 J) (Node_2 H) (Res_value 5))
(R (Resistor R12) (Node_1 A) (Node_2 J) (Res_value 4))
(R (Resistor R13) (Node_1 J) (Node_2 I) (Res_value 1))
(R (Resistor R14) (Node_1 A) (Node_2 I) (Res_value 2))
(R (Resistor R15) (Node_1 A) (Node_2 K) (Res_value 4))
(R (Resistor R16) (Node_1 A) (Node_2 L) (Res_value 3))
(R (Resistor R17) (Node_1 A) (Node_2 K) (Res_value 1))
(R (Resistor R18) (Node_1 K) (Node_2 M) (Res_value 2))
(R (Resistor R19) (Node_1 L) (Node_2 I) (Res_value 4))
(R (Resistor R20) (Node_1 M) (Node_2 I) (Res_value 5)))
    
;DEFINE RULES
;;Series Reduction
(defrule series
    ?R1 <- (R
        (Resistor ?n1)
        (Node_1 ?Node_1)
        (Node_2 ?Node_2)
        (Res_value ?r1))
    ?R2 <- (R
        (Resistor ?n2)
        (Node_1 ?Node_2)
        (Node_2 ?node3)
        (Res_value ?r2))
    (not 
        (or 
            (R
                (Resistor ?n4&~?n2&~?n1)
                (Node_1 ?Node_2))
            (R
                (Resistor ?n4&~?n1&~?n2)
                (Node_2 ?Node_2))))
    =>
    (modify ?R1
        (Node_2 ?node3)
        (Res_value (+ ?r1 ?r2)))
    (retract ?R2))


;;Parallel Reduction
(defrule parallel
    ?R1 <- (R(Resistor ?n1)
        (Node_1 ?Node_1)
        (Node_2 ?Node_2)
        (Res_value ?r1))
    ?R2 <- (R(Resistor ?n2)
        (Node_1 ?Node_1)
        (Node_2 ?Node_2)
        (Res_value ?r2))
    (test (neq ?n1 ?n2))
    =>
    (modify ?R1
(Res_value
            (/ (* ?r1 ?r2)(+ ?r1 ?r2))))
    (retract ?R2))

;;Delta-Star Transformation
(defrule delta-star
    ?R1 <- (R(Resistor ?n1)
        (Node_1 ?Node_1)
        (Node_2 ?Node_2)
        (Res_value ?r1))
    ?R2 <- (R
        (Resistor ?n2)
        (Node_1 ?Node_2)
        (Node_2 ?node3)
        (Res_value ?r2))
    ?R3 <- (R
        (Resistor ?n3)
        (Node_1 ?Node_1)
        (Node_2 ?node3)
        (Res_value ?r3))
    (or
        (R
            (Resistor ?n4 & ~?n1&~?n2&~?n3)
            (Node_2 ?Node_2)))
    =>
    (modify ?R1
    (Node_2 (str-cat ?Node_1 ?Node_2))
        (Res_value (/ (* ?r1 ?r3) (+ ?r1 ?r2 ?r3))))
    (modify ?R2
        (Node_2 (str-cat ?Node_1 ?Node_2))
        (Node_1 ?Node_2)
        (Res_value (/ (* ?r1 ?r2) (+ ?r1 ?r2 ?r3))))
    (modify ?R3
        (Node_1 (str-cat ?Node_1 ?Node_2))
        (Res_value (/ (* ?r2 ?r3) (+ ?r1 ?r2 ?r3)))))
;;Equivalence Resistance
(defrule  Req 
    ?R1 <- (R (Resistor ?n1)
        (Node_1 A)
        (Node_2 E)
        (Res_value ?r1))
    =>
    (printout t "The equivalent resistance, Req, for the circuit = " ?r1 "Ohms"))
(reset)
(run)
