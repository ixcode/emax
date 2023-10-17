

(setq incanter-temp-chart-file "/tmp/chart.png")
(setq incanter-wait-time 500)

(defun incanter-display-image-inline (buffer-name file-name)
  "Use `BUFFER-NAME' to display the image in `FILE-NAME'.
  Checks weather `BUFFER-NAME' already exists, and if not create
  as needed."
  (switch-to-buffer-other-window buffer-name)
  (iimage-mode t)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  ;; unless we clear the cache, the same cached image will
  ;; always get re-displayed.
  (clear-image-cache nil)
  (insert-image (create-image file-name))
  (read-only-mode t))
;; http://blog.brunobonacci.com/2016/03/18/emacs-incanter-hack/
(defun incanter-eval-and-display-chart ()
  "Evaluate the expression preceding point and display the chart into a popup buffer"
  (interactive)
  (let ((old-buf (current-buffer)))
    (condition-case nil
                    (delete-file incanter-temp-chart-file)
                    (error nil))
    (cider-eval-last-sexp)
    (sleep-for 0 incanter-wait-time)
    (incanter-display-image-inline "*incanter-chart*" incanter-temp-chart-file)
    (switch-to-buffer-other-window old-buf)))

(define-key cider-mode-map
    (kbd "C-c C-i") #'incanter-eval-and-display-chart)
