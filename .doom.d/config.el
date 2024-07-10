;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;;(setq user-full-name "John Doe"
 ;;     user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "FiraCode NerdFont" :size 17 :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq confirm-kill-emacs nil)
(setq scroll-margin 7)
(setq display-line-numbers-type 'relative)
(setq highlight-indent-guides-responsive 'stack)
(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-auto-stack-odd-face-perc 100)
(setq highlight-indent-guides-auto-stack-even-face-perc 80)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override
   `(+workspace-current-name))
)
;; (setq persp-emacsclient-init-frame-behaviour-override `(if (not (eq? "none" (+workspace-current-name))) (+workspace-current-name) "justWorkPlease"))

;; to show workspaces while minibuffer is not busy
(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))

(defun +vterm/named-here (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sVTerm new buffer name: ")
  (+vterm/here nil)
  (rename-buffer buffer-name t))

(map! :n "SPC o C-t" #'+vterm/named-here)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(setq LaTeX-indent-level 4)
(setq LaTeX-item-indent 4)
(setq TeX-brace-indent-level 4)
(setq indent-tabs-mode t)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)
;; (after! yasnippet
;;   (setq yas-snippet-dirs (remq 'yasnippet-snippets-dir yas-snippet-dirs)))
;; (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

(map! :desc "Try complete snippet"
      :map yas-minor-mode-map "M-TAB" yas-maybe-expand)

(setq-default flycheck-disabled-checkers '(c/c++-clang c++-cppcheck c/c++-gcc))

;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom
;; (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))

;;(setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
;;(add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet)))
;; (after! ccls
;;   (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2))
(setq lsp-clients-clangd-args '("-j=3"
				"--background-index"
				"--clang-tidy"
				"--completion-style=detailed"
				"--header-insertion=never"
				"--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

; to debug with DAP-MODE
(setq dap-auto-configure-mode t)
;; (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))

(setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java")

(setq lsp-clients-clangd-args '("-j=3"
				"--background-index"
				"--clang-tidy"
				"--completion-style=detailed"
				"--header-insertion=never"
				"--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(setq! ranger-override-dired-mode t)
(after! ranger
    (setq! ranger-override-dired 'ranger)
    (setq! ranger-preview-file t)
    ;; (setq! ranger-show-literal nil)
    ;; (setq! ranger-cleanup-eagerly t)
    (setq! ranger-show-hidden t))

;; (use-package! evil-xkbswitch
;;     :config (evil-xkbswitch-mode 1))

(use-package! reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(modify-frame-parameters nil '((inhibit-double-buffering . t)))

(defun xwidget-webkit-scroll-down-line (&optional n)
  (interactive "p" xwidget-webkit-mode)
  (xwidget-webkit-scroll-down))
(defun xwidget-webkit-scroll-up-line (&optional n)
  (interactive "p" xwidget-webkit-mode)
  (xwidget-webkit-scroll-up))

(setq haskell-process-args-ghci '("-ferror-spans" "-ignore-dot-ghci"))

(map! :desc "Evil set marker"
      :n "m" #'evil-set-marker)



(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

(after! dap-mode
    (setq dap-python-debugger 'debugpy)
    (require 'dap-python)
    (dap-register-debug-template "Python :: SailFish :: Contractlint"
        (list :type "python"
            :args "-c ../../test_cases/reentrancy/mutex_fp_prunning_non_reentrant.sol -o . -r range -p DAO,TOD -oo -sv cvc4"
            :cwd nil
            :module nil
            :program nil
            :request "launch"
            :name "Python :: Run file with args (buffer)")))

(setq lsp-pyright-diagnostic-mode "workspace")
