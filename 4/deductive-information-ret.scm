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

