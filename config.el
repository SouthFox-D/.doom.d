(setq doom-theme 'ef-kassio)

(setq user-full-name "SouthFox"
      user-mail-address "master@southfox.me")

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

(after! org
  (setq org-directory "~/org/"
        org-image-actual-width '(400)
        org-link-search-must-match-exact-headline nil
        org-log-done 'time
        org-log-into-drawer t))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        )
  )

(setq org-agenda-files '("~/Sync/org/GTD/inbox.org"
                         "~/Sync/org/GTD/gtd.org"
                         "~/Sync/org/GTD/tickler.org"))

(after! org
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file "~/Sync/org/GTD/inbox.org")
                               "* TODO %i%? \n SCHEDULED: %t")
                              ("T" "Tickler" entry
                               (file+headline "~/Sync/org/GTD/tickler.org" "Tickler")
                               "* %i%? \n %U"))))

(setq org-roam-directory "~/Sync/org/Note/org-roam")
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "references" plain "%?"
         :if-new
         (file+head "references/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("t" "ttk" plain "%?"
         :if-new
         (file+head "ttk/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+date: %T\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+date: %T\n#+filetags: :article: :publish:\n#+hugo_auto_set_lastmod: t\n")
         :immediate-finish t
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            ":PROPERTIES:
:header-args:  :exports both\n:END:
#+title: %<%Y-%m-%d>\n#+date: %T\n#+hugo_auto_set_lastmod: t\n"))))

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

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

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

(defun my/org-roam-publish ()
  "Publish current file"
  (interactive)
  (org-roam-set-keyword "filetags" ":publish:")
  (save-buffer)
  (org-hugo-export-wim-to-md))

(defun my/org-roam-creat-node ()
  "creat node and add IS_NODE property"
  (interactive)
  (org-id-get-create)
  (org-set-tags ":NODE:")
  (save-buffer)
  (org-hugo-export-wim-to-md))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(setq cnfonts-personal-fontnames
      '(()
        ()
        ("Noto Color Emoji")
        ()
        ()))
(cnfonts-mode 1)

(setq word-wrap-by-category t)

(setq doom-unicode-font (font-spec :family "Noto Color Emoji"))
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 102))

(add-hook 'python-mode-hook  #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)

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

(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

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

(load! "secrets")
(setq-default custom-file (expand-file-name "secrets.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq bibtex-completion-pdf-field "File")
(setq bibtex-completion-bibliography '("~/Sync/Ebook/catalog.bib"))
(setq citar-bibliography '("~/Sync/Ebook/catalog.bib"))

(setq calibredb-root-dir "~/Documents/Ebook")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
(setq calibredb-format-all-the-icons t)

(map! :map calibredb-show-mode-map
      :n "?" #'calibredb-entry-dispatch
      :n "o" #'calibredb-find-file
      :n "O" #'calibredb-find-file-other-frame
      :n "V" #'calibredb-open-file-with-default-tool
      :n "s" #'calibredb-set-metadata-dispatch
      :n "e" #'calibredb-export-dispatch
      :n "q" #'calibredb-entry-quit
      :n "." #'calibredb-open-dired
      :n [tab] #'calibredb-toggle-view-at-point
      :n "M-t" #'calibredb-set-metadata--tags
      :n "M-a" #'calibredb-set-metadata--author_sort
      :n "M-A" #'calibredb-set-metadata--authors
      :n "M-T" #'calibredb-set-metadata--title
      :n "M-c" #'calibredb-set-metadata--comments)
(map! :map calibredb-search-mode-map
      :n [mouse-3] #'calibredb-search-mouse
      :n "RET" #'calibredb-find-file
      :n "?" #'calibredb-dispatch
      :n "a" #'calibredb-add
      :n "A" #'calibredb-add-dir
      :n "c" #'calibredb-clone
      :n "d" #'calibredb-remove
      :n "D" #'calibredb-remove-marked-items
      :n "j" #'calibredb-next-entry
      :n "k" #'calibredb-previous-entry
      :n "l" #'calibredb-virtual-library-list
      :n "L" #'calibredb-library-list
      :n "n" #'calibredb-virtual-library-next
      :n "N" #'calibredb-library-next
      :n "p" #'calibredb-virtual-library-previous
      :n "P" #'calibredb-library-previous
      :n "s" #'calibredb-set-metadata-dispatch
      :n "S" #'calibredb-switch-library
      :n "o" #'calibredb-find-file
      :n "O" #'calibredb-find-file-other-frame
      :n "v" #'calibredb-view
      :n "V" #'calibredb-open-file-with-default-tool
      :n "." #'calibredb-open-dired
      :n "b" #'calibredb-catalog-bib-dispatch
      :n "e" #'calibredb-export-dispatch
      :n "r" #'calibredb-search-refresh-and-clear-filter
      :n "R" #'calibredb-search-clear-filter
      :n "q" #'calibredb-search-quit
      :n "m" #'calibredb-mark-and-forward
      :n "f" #'calibredb-toggle-favorite-at-point
      :n "x" #'calibredb-toggle-archive-at-point
      :n "h" #'calibredb-toggle-highlight-at-point
      :n "u" #'calibredb-unmark-and-forward
      :n "i" #'calibredb-edit-annotation
      :n "DEL" #'calibredb-unmark-and-backward
      :n [backtab] #'calibredb-toggle-view
      :n [tab] #'calibredb-toggle-view-at-point
      :n "M-n" #'calibredb-show-next-entry
      :n "M-p" #'calibredb-show-previous-entry
      :n "/" #'calibredb-search-live-filter
      :n "M-t" #'calibredb-set-metadata--tags
      :n "M-a" #'calibredb-set-metadata--author_sort
      :n "M-A" #'calibredb-set-metadata--authors
      :n "M-T" #'calibredb-set-metadata--title
      :n "M-c" #'calibredb-set-metadata--comments)

(setq sdcv-say-word-p t)
(setq   sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic/"))

(map! :leader :desc "sdvc" "z" #'sdcv-search-pointer+)

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

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

(beacon-mode 1)

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
(map! :map ement-room-list-mode-map
      :n "RET" #'ement-room-list-RET
      :n "q"   #'kill-current-buffer
      )

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

(setq leetcode-save-solutions t)
(setq leetcode-directory "~/Documents/leetcode")

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t))

(use-package! screenshot
  :defer t)

(set-email-account! "southfox.me"
  '((mu4e-sent-folder       . "/Sent")
    (mu4e-drafts-folder     . "/southfox.me/Drafts")
    (mu4e-trash-folder      . "/southfox.me/Trash")
    (mu4e-refile-folder     . "/southfox.me/All Mail")
    (smtpmail-smtp-user     . "master@southfox.me")
    (mu4e-compose-signature . "---\nFor mu4e"))
  t)

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! elfeed
  (setq elfeed-search-filter "@6-month-ago +unread")
  (setq rmh-elfeed-org-files '("~/Sync/org/rss.org"))
  (setq elfeed-curl-extra-arguments '("-H Mozilla/5.0 (Windows NT 10.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.7113.93 Safari/537.36"
                                      "--proxy" "socks5://127.0.0.1:10808"
                                      "--retry" "2"
                                      "--insecure")))

(use-package! flycheck-clj-kondo
  :after (clojure-mode))

(use-package! clj-refactor
  :after (clojure-mode))

(use-package! paredit
  :after (clojure-mode)
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

(use-package! lsp-tailwindcss)
