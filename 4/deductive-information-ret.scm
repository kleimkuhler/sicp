;; Query input
;; Match the certain pattern
(job ?x (computer programmer))

;; 4.55
(supervisor ?name (Bitdiddle Ben))
(job ?name (accounting . ?title))
(address ?name (Slumerville . ?street-address))

;; 4.56
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))
(and (salary (Bitdiddle Ben) ?salary-ben)
     (salary ?name ?salary-any)
     (lisp-value < ?salary-any ?salary-ben))
(and (supervisor ?name ?supervisor)
     (job ?supervisor ?job-supervisor)
     (not (job ?supervisor (computer . ?title))))

;; Rules (rule <conclusion> <body>)
(rule (same ?x ?x))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (supervisor ?staff-person ?middle-manager)
	       (outranked-by ?middle-manager ?boss))))

;; 4.57
(rule (can-do-job ?job-1 ?job-2)
      (lisp-value sublist? ?job-2 ?job-1))

(rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?titles-1)
	   (job ?person-2 ?titles-2)
	   (not (same ?person-1 ?person-2))
	   (or (same ?titles-1 ?titles-2)
	       (can-do-job ?tiles-1 ?titles-2))))

;; 4.62
(rule (last-pair (?x) (?x)))

(rule (last-pair (?y . ?z) (?x))
      (last-pair ?z (?x)))

;; 4.63
(rule (grandson ?s ?g)
      (and (father ?s ?f)
	   (father ?f ?g)))

;; 4.64
;; Infinite loop introduced here
(and (outranked-by ?middle-manager ?boss)
     (supervisor ?staff-person ?middle-manager))

;; 4.65
;; Each path taken reports a separate result

;; 4.68
(rule (reverse? () ()))

(rule (reverse? (?x . ?y) ?z)
      (and (append-to-form ?u ?x ?z)
           (reverse? ?u ?y)))

