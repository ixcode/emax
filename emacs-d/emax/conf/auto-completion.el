;; auto-completion.el
;; configures auto completion


;; Default to using ido because it stays in the mini-buffer unless you press tab
;; Also can use "ivy-conf.el" for an alternative

(defconst *mode* "ido")  ;; or "ivy" or "helm"
(load (concat *mode* "-conf"))


