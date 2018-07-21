; Insert Stobie Poles v1.0
; Author: Matti Syrjanen - Distribution Power Design
; This program automatically inserts stobie poles on a drawing to the coordinates and orientation specified in the PLS-CADD staking table.
; Ensure that the headings and extra rows in the end of the CSV file are removed.

(defun c:SAPNINSERTPOLESFROMCSV ()
	(insertpoles "LD-OH-POLE" "Pole Angle" "POLE-NO")
)

(defun c:DPDINSERTEXISTINGPOLESFROMCSV ()
	(insertpoles "DPD Stobie Pole Multi Anno" "Angle1" "1")
)

(defun c:DPDINSERTPROPOSEDPOLESFROMCSV ()
	(insertpoles "DPD Stobie Pole Proposed Multi Anno" "Angle1" "1")
)

(defun insertpoles (blockname anglePropertyName poleNumberAttName / data file)
	(vl-load-com)
	(if (not (tblsearch "block" blockname)) (alert (strcat "Please load block '" blockname "'."))
		(cond ((setq file (getfiled "Select CSV File" "" "csv" 16)) ; Select file
			(setq oldsnap (getvar "osmode"))
			(setvar "osmode" 0)
			(setq data (LM:readcsv file)) ; Import CSV file
			; Parse CSV file line by line
			(foreach line data
				(vla-insertblock
					(if
						(or (eq acmodelspace
								(vla-get-activespace
									(cond (*AcadDoc*)
										((setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object))))
									)
								)
							)
							(eq :vlax-true (vla-get-mspace *AcadDoc*))
						)
						(vla-get-modelspace *AcadDoc*)
						(vla-get-paperspace *AcadDoc*)
					)
					
					; Set coordinates
					(vlax-3d-point (list 
										(atof (nth 6 line)) ; X  Easting   (m)
										(atof (nth 7 line)) ; Y  Northing   (m)
										0					; Z coordinate
									)
					)
					blockname
					1.
					1.
					1.
					0 ; Rotation
				)

				(LM:setattributevalue (entlast) poleNumberAttName (nth 1 line))
				(LM:setdynpropvalue (vlax-ename->vla-object (entlast)) anglePropertyName (dtr (- 90 (atof (nth 13 line)))))
			)
			(setvar "osmode" oldsnap))
		)
	)
    (princ)
)

;; Read CSV  -  Lee Mac
;; Parses a CSV file into a matrix list of cell values.
;; csv - [str] filename of CSV file to read
 
(defun LM:readcsv ( csv / des lst sep str )
    (if (setq des (open csv "r"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (while (setq str (read-line des))
                (setq lst (cons (LM:csv->lst str sep 0) lst))
            )
            (close des)
        )
    )
    (reverse lst)
)

;; CSV -> List  -  Lee Mac
;; Parses a line from a CSV file into a list of cell values.
;; str - [str] string read from CSV file
;; sep - [str] CSV separator token
;; pos - [int] initial position index (always zero)
(defun LM:csv->lst ( str sep pos / s )
    (cond
        ((not (setq pos (vl-string-search sep str pos)))
            (if (wcmatch str "\"*\"")
                (list (LM:csv-replacequotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        ((or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
            (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
        )
            (LM:csv->lst str sep (+ pos 2))
        )
        (   (wcmatch s "\"*\"")
            (cons
                (LM:csv-replacequotes (substr str 2 (- pos 2)))
                (LM:csv->lst (substr str (+ pos 2)) sep 0)
            )
        )
        (   (cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
    )
)

(defun LM:csv-replacequotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
)

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:setattributevalue ( blk tag val / end enx )
    (while
        (and
            (null end)
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget (setq blk (entnext blk)))))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 enx) enx))
                (progn
                    (entupd blk)
                    (setq end val)
                )
            )
        )
    )
)

; converts degrees to radians. 
(defun dtr (a) (* pi (/ a 180.0))) 

; converts radians to degrees
(defun rtd (r) (* 180.0 (/ r pi)))
