;;; package --- Summary
;;coding.el - Provides support for various coding languages
;;; Commentary:
;;; Code:

(require 'quelpa-use-package)
(use-package chatgpt
  :quelpa ((chatgpt :fetcher git :url "https://github.com/joshcho/ChatGPT.el.git") :upgrade t)
  :bind ("C-c q" . chatgpt-query))

(setq chatgpt-code-query-map
      '(
        ;; ChatGPT.el defaults, string for each shortcut
        ("bug" . "There is a bug in the following, please help me fix it.")
        ("doc" . "Please write the documentation for the following.")
        ("improve" . "Please improve the following.")
        ("understand" . "What is the following?")
        ("refactor" . "Please refactor the following.")
        ("suggest" . "Please make suggestions for the following.")
        ;; your shortcut
        ("prompt-name" . "My custom prompt.")))

(provide 'chat-gpt)
;;; chat-gpt.el ends here
