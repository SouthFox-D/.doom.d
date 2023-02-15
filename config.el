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
(setq doom-theme 'ef-spring)
(cnfonts-mode 1)
(beacon-mode 1)
(setq word-wrap-by-category t)

(load! "secrets")
(setq-default custom-file (expand-file-name "secrets.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

;;clippy
(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

;;shell
(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)

;;font
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 102))

;; (setq doom-font (font-spec :size 16)
;;       doom-big-font (font-spec :size 24)
;;       doom-variable-pitch-font (font-spec :size 16))

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

;; debuger
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;;; eval
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

;; mastodon
(map! :leader
      :prefix ("o")
      :desc "Mastodon"          "M" #'mastodon)

(map! :after mastodon
      :map mastodon-mode-map
      :n "[ [" #'mastodon-tl--goto-prev-toot
      :n "] ]" #'mastodon-tl--goto-next-toot
      :n "g k" #'mastodon-tl--previous-tab-item
      :n "g j" #'mastodon-tl--next-tab-item

      :n "q" #'kill-current-buffer
      :n "Q" #'kill-buffer-and-window

      ;;; timelines
      :n "#" #'mastodon-tl--get-tag-timeline
      :n "A" #'mastodon-profile--get-toot-author
      :n "F" #'mastodon-tl--get-federated-timeline
      :n "H" #'mastodon-tl--get-home-timeline
      :n "L" #'mastodon-tl--get-local-timeline
      :n "N" #'mastodon-notifications-get
      :n "O" #'mastodon-profile--my-profile
      :n "P" #'mastodon-profile--show-user
      :n "T" #'mastodon-tl--thread

      ;;; toot actions
      :n "K" #'mastodon-toot--bookmark-toot-toggle
      :n "R" #'mastodon-toot--toggle-boost
      :n "c" #'mastodon-tl--toggle-spoiler-text-in-toot
      :n "C" #'mastodon-toot--copy-toot-url
      :n "o" #'mastodon-url-lookup
      :n "d" #'mastodon-toot--delete-toot
      :n "D" #'mastodon-toot--delete-draft-toot
      :n "f" #'mastodon-toot--toggle-favourite
      :n "r" #'mastodon-toot--reply
      :n "u" #'mastodon-tl--update
      :n "v" #'mastodon-tl--poll-vote

      ;;; toot!
      :n "t" #'mastodon-toot

      ;;; mastodon additions
      :n "S"    #'mastodon-search--search-query
      :n "V F"  #'mastodon-profile--view-favourites
      :n "V B"  #'mastodon-profile--view-bookmarks
      :n "V L" #'mastodon-tl--view-list-timeline
      )

;; mail
(set-email-account! "southfox.me"
  '((mu4e-sent-folder       . "/southfox.me/Sent Mail")
    (mu4e-drafts-folder     . "/southfox.me/Drafts")
    (mu4e-trash-folder      . "/southfox.me/Trash")
    (mu4e-refile-folder     . "/southfox.me/All Mail")
    (smtpmail-smtp-user     . "master@southfox.me")
    (mu4e-compose-signature . "---\nFor mu4e"))
  t)

;; guess-word
(use-package! guess-word)
(setq guess-word-org-file (f-expand "~/Nextcloud/Documents/my-esl.org"))

;;sdcv
(setq sdcv-say-word-p t)

(setq   sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic/"))
        ;; sdcv-dictionary-simple-list (list "懒虫简明英汉词典"
        ;;                                   "KDic11万英汉词典")
        ;; sdcv-dictionary-complete-list (list "懒虫简明英汉词典"
        ;;                                     "KDic11万英汉词典"))

(map! :leader :desc "sdvc" "z" #'sdcv-search-pointer+)

;; rime
(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (define-key rime-mode-map (kbd "C-i") 'rime-force-enable)
  (setq rime-show-candidate 'popup)
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
(parrot-mode 1)
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
;; (defun org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(after! org
  (setq org-directory "~/org/"
        org-image-actual-width '(400)
        org-link-search-must-match-exact-headline nil
        org-log-done 'time
        org-log-into-drawer t))

(add-hook 'org-pomodoro-finished-hook
                (lambda()
                (org-roam-dailies-capture-today)))

;; ob-restclient
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

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
;; roam-hugo
(setq org-hugo-base-dir "~/Documents/roam-publish/")

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (daviwil/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-export-all ()
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (dolist (org-file (my/org-roam-list-notes-by-tag "publish"))
  ;(dolist (org-file (directory-files-recursively org-roam-directory "\.org$"))
    (with-current-buffer (find-file org-file)
        (org-hugo-export-wim-to-md))))

(defun my/org-roam-creat-node ()
  "creat node and add IS_NODE property"
  (interactive)
  (org-id-get-create)
  (org-set-tags ":NODE:")
  (save-buffer)
  (org-hugo-export-wim-to-md))

;; org-roam
(setq org-roam-directory "~/Nextcloud/Note/org-roam")
(setq org-id-extra-files (org-roam--list-files org-roam-directory))
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("t" "ttk" plain "%?"
         :if-new
         (file+head "ttk/${title}.org" "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+date: %T\n#+filetags: :article: :publish:\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+date: %T\n#+hugo_auto_set_lastmod: t\n"))))

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

;; org-transclusion
(use-package! org-transclusion
              :after org
              :init
              (map!
               :map global-map "<f12>" #'org-transclusion-add
               :leader
               :prefix "n"
               :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

;; ement
(setq ement-save-sessions t
      ement-room-message-format-spec "%B%r%R%t")
(map! :map ement-room-mode-map
      :n "c e" #'ement-room-edit-message
      :n "c m" #'ement-room-send-message
      :n "c r" #'ement-room-send-reaction
      :n "c i" #'ement-room-send-image
      :n "c f" #'ement-room-send-file
      :n "q"   #'kill-current-buffer
      )
