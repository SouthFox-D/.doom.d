;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "SouthFox"
      user-mail-address "master@southfox.me")

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

;;; doom
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)
(cnfonts-mode 1)
(setq word-wrap-by-category t)

(load! "secrets")
(setq-default custom-file (expand-file-name "secrets.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

;;font
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 102))

(setq doom-font (font-spec :size 16)
      doom-big-font (font-spec :size 24)
      doom-variable-pitch-font (font-spec :size 16))

;; dashboard banner
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '("                                                                   ,           "
            "                                                             _.-=;~ /_         "
            "                                                          _-~   '     ;.       "
            "                                                      _.-~     '   .-~-~`-._   "
            "                                                _.--~~:.             --.____88 "
            "                              ____.........--~~~. .' .  .        _..-------~~  "
            "                     _..--~~~~               .' .'             ,'              "
            "                 _.-~                        .       .     ` ,'                "
            "               .'                                    :.    ./                  "
            "             .:     ,/          `                   ::.   ,'                   "
            "           .:'     ,(            ;.                ::. ,-'                     "
            "          .'     ./'.`.     . . /:::._______.... _/:.o/                        "
            "         /     ./'. . .)  . _.,'               `88;?88|                        "
            "       ,'  . .,/'._,-~ /_.o8P'                  88P ?8b                        "
            "    _,'' . .,/',-~    d888P'                    88'  88|                       "
            " _.'~  . .,:oP'        ?88b              _..--- 88.--'8b.--..__                "
            ":     ...' 88o __,------.88o ...__..._.=~- .    `~~   `~~      ~-._ Fox! _.    "
            "`.;;;:='    ~~            ~~~                ~-    -       -   -               "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

;(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn) ;)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;;mode

;; mail
(set-email-account! "southfox.me"
  '((mu4e-sent-folder       . "/southfox.me/Sent Mail")
    (mu4e-drafts-folder     . "/southfox.me/Drafts")
    (mu4e-trash-folder      . "/southfox.me/Trash")
    (mu4e-refile-folder     . "/southfox.me/All Mail")
    (smtpmail-smtp-user     . "master@southfox.me")
    (mu4e-compose-signature . "---\nFor mu4e"))
  t)

(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function #'message-send-mail-with-sendmail)

;; guess-word
(use-package! guess-word)
(setq guess-word-org-file (f-expand "~/Nextcloud/Documents/my-esl.org"))

;;sdcv
(setq sdcv-say-word-p t)
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic"))

(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "KDic11万英汉词典"))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
        "懒虫简明英汉词典"
        "KDic11万英汉词典"
        ))

(map! :leader :desc "sdvc" "d" #'sdcv-search-pointer+)
;; rime
(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (define-key rime-mode-map (kbd "C-i") 'rime-force-enable)
  (setq rime-show-candidate 'posframe)
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-current-uppercase-letter-p
          rime-predicate-punctuation-line-begin-p)))

;; ace-pinyin
(use-package! ace-pinyin
  :config
  (ace-pinyin-global-mode +1))
;; parrot
(define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
(define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)

(setq parrot-rotate-dict
      '(
        (:rot ("yes" "no") :caps t :upcase t)
        (:rot ("&" "|"))
        (:rot ("begin" "end") :caps t :upcase t)
        (:rot ("enable" "disable") :caps t :upcase t)
        (:rot ("enter" "exit") :caps t :upcase t)
        (:rot ("forward" "backward") :caps t :upcase t)
        (:rot ("front" "rear" "back") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("start" "stop") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("if" "else" "elif"))
        (:rot ("ifdef" "ifndef"))
        ))

(add-hook 'emacs-startup-hook
          (lambda ()
          (setq parrot-num-rotations 10)
          (parrot-mode)
          (parrot-set-parrot-type 'emacs)))

(defun my-parrot-thumbsup-play ()
  (parrot-set-parrot-type 'thumbsup)
  (parrot-start-animation))

(defun my-parrot-emacs-play ()
  (parrot-set-parrot-type 'emacs)
  (parrot-start-animation))

(defun my-parrot-rotating-play ()
  (parrot-set-parrot-type 'rotating)
  (parrot-start-animation))

(my-add-to-multiple-hooks
 'my-parrot-thumbsup-play
 '(org-after-todo-state-change-hook
   org-clock-in-hook
   org-timer-done-hook
   git-commit-post-finish-hook))

(my-add-to-multiple-hooks
 'my-parrot-emacs-play
 '(after-save-hook
   find-file-hook))

(my-add-to-multiple-hooks
 'my-parrot-rotating-play
 '(elfeed-search-mode-hook
   mu4e-main-mode-hook))

;;; org
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(after! org
  (setq org-directory "~/org/")
  (setq org-image-actual-width '(400))
  (setq org-link-search-must-match-exact-headline nil)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t))

;(setq org-emphasis-regexp-components '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
;(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;(org-element-update-syntax)

;; super agenda
(use-package! org-super-agenda
  :commands org-super-agenda-mode)

;; (after! org-agenda
;;   (org-super-agenda-mode))

;; (setq org-agenda-skip-scheduled-if-done t
;;       org-agenda-skip-deadline-if-done t
;;       org-agenda-include-deadlines t
;;       org-agenda-block-separator nil
;;       org-agenda-tags-column 100 ;; from testing this seems to be a good value
;;       org-agenda-compact-blocks t)

;; (setq org-agenda-custom-commands
;;       '(("o" "Overview"
;;          ((agenda "" ((org-agenda-span 'day)
;;                       (org-agenda-start-day nil)
;;                       (org-super-agenda-groups
;;                        '((:name "降"
;;                           :time-grid t
;;                           :date today
;;                           :scheduled today
;;                           :order 1)))))
;;           (alltodo "" ((org-agenda-overriding-header "")
;;                        (org-super-agenda-groups
;;                         '((:name "继"
;;                            :todo "STRT"
;;                            :order 1)
;;                           (:name "急"
;;                            :tag "急"
;;                            :priority "A"
;;                            :order 6)
;;                           (:name "绷"
;;                            :deadline today
;;                            :order 2)
;;                           (:name "临"
;;                            :deadline future
;;                            :order 8)
;;                           (:name "逝"
;;                            :deadline past
;;                            :face error
;;                            :order 7)
;;                           (:name "待"
;;                            :scheduled today
;;                            :order 3)
;;                           (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

;; elfeed
 (after! elfeed
  (setq elfeed-search-filter "@6-month-ago +unread")
  (setq rmh-elfeed-org-files '("~/Nextcloud/rss/rss.org"))
  (setq elfeed-curl-extra-arguments '("-H Mozilla/5.0 (Windows NT 10.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.7113.93 Safari/537.36"
                                      "--proxy" "socks5://127.0.0.1:10808"
                                      "--retry" "2"
                                      "--insecure")))

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

;; org-fragtog
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; org-capture
(setq org-agenda-files '("~/Nextcloud/gtd/inbox.org"
                         "~/Nextcloud/gtd/gtd.org"
                         "~/Nextcloud/gtd/tickler.org"))

(after! org
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file "~/Nextcloud/gtd/inbox.org")
                               "* TODO %i%? \n SCHEDULED: %t")
                              ("T" "Tickler" entry
                               (file+headline "~/Nextcloud/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U"))))

;; org-roam
(setq org-roam-directory "~/Nextcloud/Note/org-roam")
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("t" "ttk" plain "%?"
         :if-new
         (file+head "ttk/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))

;; org-roam-ui
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;         a hookable mode anymore, you're advised to pick something yourself
;         if you don't care about startup time, use
;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq-local epa-file-encrypt-to '("master@southfox.me"))

;; org-download
(require 'org-download)
(setq org-download-screenshot-method "flameshot gui --raw >%s")
(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "./images")
(defun dummy-org-download-annotate-function (link)
  "")
(setq org-download-annotate-function
      #'dummy-org-download-annotate-function)

;; org-transclusion
(use-package! org-transclusion
              :after org
              :init
              (map!
               :map global-map "<f12>" #'org-transclusion-add
               :leader
               :prefix "n"
               :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))
