;;; electric-pair-conf.el
;; Automatically mathes anything like braces or quotes
(electric-pair-mode 1)

 
;; Not sure we want single quotes enabled for elisp
;; TODO - work out how to disable just for elisp
;;(push '(?\' . ?\') electric-pair-pairs)      
(push '(?\' . ?\') electric-pair-text-pairs) 

(push '(?\{ . ?\}) electric-pair-pairs)      
(push '(?\{ . ?\}) electric-pair-text-pairs)

(setq electric-pair-delete-adjacent-pairs t)

