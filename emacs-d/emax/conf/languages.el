;;; languages.el --- Sets up useful things for programming

;;; Commentary:
;; See http://shallowsky.com/blog/linux/editors/emacs-tab-separated-files.html

;;; Code:
;; (when (not (package-installed-p 'docker))
;;   (package-install 'docker))

(when (not (package-installed-p 'csv-mode))
  (package-install 'csv-mode))

(when (not (package-installed-p 'fish-mode))
  (package-install 'fish-mode))


(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;;(add-to-list 'auto-mode-alist '("\\.[Tt][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; This is a bit hacky as it will only work when you load so if you switch between file types it won't work.
;; (defun csv-mode-setup-filetype ()
  
;;   (when (string= (file-name-extension buffer-file-name) "csv")
;;     (setq csv-separators '(","))
;;     (message "Trying to setup csv-mode to use commas"))
;;   (when (string= (file-name-extension buffer-file-name) "tsv")
;;     (setq csv-separators '("\t"))
;;     (message "Trying to setup csv-mode to use tabs")))

;; (add-hook 'csv-mode-hook 'csv-mode-setup-filetype)

;;(setq csv-separators '("\t"))

;; Extended from http://shallowsky.com/blog/linux/editors/emacs-tab-separated-files.html
;; (define-derived-mode tabbed-mode csv-mode "TSV"
;;   (setq-local csv-separators '("\t"))
;;   (local-set-key (kbd "TAB") 'self-insert-command))

(autoload 'tsv-mode "tsv-mode"
  "Major mode for editing tab-separated value files." t)


;;(add-to-list 'auto-mode-alist '("\\.[Tt][Ss][Vv]\\'" . tsv-mode))
;;(autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
;;(autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)

(when (not (package-installed-p 'find-file-in-project))
  (package-install 'find-file-in-project))

;; (when (not (package-installed-p 'solidity-mode))
;;   (package-install 'solidity-mode))

;; (require 'solidity-mode)

;; (when (not (package-installed-p 'graphviz-dot-mode))
;;   (package-install 'graphviz-dot-mode))

;; (require 'graphviz-dot-mode)
;; (add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

;; (when (not (package-installed-p 'dot-mode))
;;   (package-install 'dot-mode))

;; (require 'dot-mode)
;; (add-hook 'find-file-hooks 'dot-mode-on)

(when (not (package-installed-p 'yaml-mode))
  (package-install 'yaml-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(when (not (package-installed-p 'rainbow-delimiters))
  (package-install 'rainbow-delimiters))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when (not (package-installed-p 'rainbow-mode))
  (package-install 'rainbow-mode))

(require 'rainbow-mode)
;;(rainbow-mode 0)

;;https://stackoverflow.com/questions/16048231/how-to-enable-a-non-global-minor-mode-by-default-on-emacs-startup
(define-globalized-minor-mode emax-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))


(setq completion-fail-discreetly t) ;; prevents annoying messages in the minibuffer

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "λ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\)("
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "ƒ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\){"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "∈")
;;                                nil))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

;; (when (not (package-installed-p 'tabbar))
;;   (package-install 'tabbar))

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

(unless (package-installed-p 'cider)
  (package-install 'cider))

(require 'cider)
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

(add-hook 'cider-interaction-mode-hook
          'cider-turn-on-eldoc-mode)

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*cider*")
(add-hook 'cider-mode-hook 'subword-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)

;; https://github.com/syohex/emacs-git-gutter
;; (when (not (package-installed-p 'git-gutter))
;;   (package-install 'git-gutter))
;; This doesnt seem to work
;; (require 'git-gutter)
;; (global-git-gutter-mode t)

(when (not (package-installed-p 'paredit))
  (package-install 'paredit))

(when (not (package-installed-p 'clojure-mode))
  (package-install 'clojure-mode))


(when (not (package-installed-p 'nav))
  (package-install 'nav))

;; (when (not (package-installed-p 'itail))
;;   (package-install 'itail))

;;(require 'itail)

;; (when (not (package-installed-p 'jedi))
;;   (package-install 'jedi))

;; (require 'jedi)
;; (setq python-python-command "/usr/local/bin/python3")
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional


;;(when (not (package-installed-p 'multi-web-mode))(package-install 'multi-web-mode))
;;(require 'multi-web-mode)

(when (not (package-installed-p 'haml-mode))
  (package-install 'haml-mode))
(require 'haml-mode)

(when (not (package-installed-p 'slim-mode))
  (package-install 'slim-mode))
(require 'slim-mode)

(when (not (package-installed-p 'lua-mode))
  (package-install 'lua-mode))
(require 'lua-mode)


;; (when (not (package-installed-p 'neotree))
;;   (package-install 'neotree))
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)







;;(setq mweb-default-major-mode 'html-mode)

;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;(multi-web-global-mode 1)




(require 'paredit)
(autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code." t)

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))

(when (not (package-installed-p 'sass-mode))
  (package-install 'sass-mode))

(require 'sass-mode)

;; On Emacs earlier than 24 can't install like this, need to follow instructions at ;; https://magit.github.io/
(when (not (package-installed-p 'magit))
  (package-install 'magit))


(require 'magit)

;; (require 'lambda-mode)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (lambda-mode)))
;; (add-hook 'lisp-mode-hook             (lambda () (lambda-mode)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (lambda-mode)))
;; (add-hook 'clojure-mode-hook (lambda () (lambda-mode)))


;;(load "pretty-lambda.el")
;; lambda mode seems to work well
;;(load "eshell.el")
(load "tidy.el")

(defun my-comint-init ()
           (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

(require 'tidy)


(when (not (package-installed-p 'markdown-mode))
  (package-install 'markdown-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode 'goto-address-mode)


(setq explicit-shell-file-name "/bin/bash")

(load "nodejs-mode.el")
(require 'nodejs-mode)

;; Autocomplete dependencies...

(load "popup.el")
(require 'popup)

(load "fuzzy.el")
(require 'fuzzy)

(when (not (package-installed-p 'auto-complete))
  (package-install 'auto-complete))

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(load "auto-complete.el")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(require 'auto-complete-config)
(ac-config-default)

;;(setq ac-comphist-file (concat "~/tmp/" "ac-comphist.dat"))

(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)



(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))


;; (when (not (package-installed-p 'ac-nrepl))
;;   (package-install 'ac-nrepl))

;; (require 'ac-nrepl)

;; (when (not (package-installed-p 'slime))
;;   (package-install 'slime))

;; (when (not (package-installed-p 'ac-slime))
;;   (package-install 'ac-slime))

;; (require 'ac-slime)

;; (when (not (package-installed-p 'pos-tip))
;;   (package-install 'pos-tip))

;;(require 'pos-tip)

;;(add-hook 'slime-mode-hook 'set-up-slime-ac)


;; (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
;;                 sass-mode yaml-mode csv-mode espresso-mode haskell-mode
;;                 html-mode nxml-mode sh-mode smarty-mode clojure-mode nrepl-mode
;;                 lisp-mode textile-mode tuareg-mode))
;;   (add-to-list 'ac-modes mode))

;; (ac-flyspell-workaround)
;; auto-complete
;; Examples



;;;;Key triggers
;; (define-key ac-completing-map (kbd "C-M-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
;; (define-key ac-completing-map "\t" 'ac-complete)
;; (define-key ac-completing-map (kbd "M-RET") 'ac-help)
;; (define-key ac-completing-map "\r" 'nil)



(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)


;;nREPL

(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)


;;Auto Complete
;;(live-add-pack-lib "ac-nrepl")
;;(require 'ac-nrepl )
;;(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;;(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

;;; Monkey Patch nREPL with better behaviour:
;; Well it works if you have the other functions
;; (defun nrepl-region-for-expression-at-point ()
;;   "Return the start and end position of defun at point."
;;   (when (and (live-paredit-top-level-p)
;;              (save-excursion
;;                (ignore-errors (forward-char))
;;                (live-paredit-top-level-p)))
;;     (error "Not in a form"))

;;   (save-excursion
;;     (save-match-data
;;       (ignore-errors (live-paredit-forward-down))
;;       (paredit-forward-up)
;;       (while (ignore-errors (paredit-forward-up) t))
;;       (let ((end (point)))
;;         (backward-sexp)
;;         (list (point) end)))))

(setq nrepl-port "6678")


(when (not (package-installed-p 'yasnippet))
  (package-install 'yasnippet))

(when (not (package-installed-p 'go-mode))
  (package-install 'go-mode))

;; (when (not (package-installed-p 'angular-snippets))
;;   (package-install 'angular-snippets))


;;(require 'yasnippet)
;;(yas-global-mode 1)


 
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(when (not (package-installed-p 'web-mode))
  (package-install 'web-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-engines-alist
      '(("php" . "\\.php\\'")
        ("blade"  . "\\.blade\\.")
        ("django" . "\\.html\\'")))


(when (not (package-installed-p 'flycheck))
  (package-install 'flycheck))

(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))

(global-flycheck-mode)

(require 'nodejs-repl)


;;(require 'kotlin-mode)

;;; Python
;;; https://docs.python-guide.org/starting/install/osx/



(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(when (not (package-installed-p 'elpy))
  (package-install 'elpy))

(require 'elpy)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

(elpy-enable)

(add-hook
     'python-mode-hook
      (lambda ()
        (local-set-key (kbd "C-x C-e") 'elpy-shell-send-statement-and-step)))

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Note, thanks to https://www.reddit.com/r/emacs/comments/3c562f/using_emacs_elpy_flycheckpylint_how_to_disable/
;; You need to put the following in a file called ~/.config/flake8 in order to turn off "line too long warnings"
;; [flake8]
;; ignore = E221,E501,E203,E202,E272,E251,E211,E222,E701
;; max-line-length = 160
;; exclude = tests/*
;; max-complexity = 10

(provide 'languages)
;;; languages.el ends here
