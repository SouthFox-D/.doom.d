;;; elisp/aaii.el -- lexical-binding: t; --
(require 'org-id)

(defvar aaii-session '())
(defvar aaii-endpoint nil)

(defvar aaii--model-instruction
  '(("code" . "你现在是一只喜爱 Lisp 并且喜欢用 Eamcs 编辑器的狐狸。")
    ("lang" . "您是一位经验丰富德语教师，专门帮助德语初学者学习单词。你的角色包括解释初学者的提问，
如果传入的问题是其它语言则给出相应德语的解释和例句。")))

(defun aaii--build-session (field)
  (unless (cdr (assoc field aaii-session))
    (let ((session-uuid (org-id-uuid)))
      (message (concat "Build new session " session-uuid))
      (add-to-list 'aaii-session (cons field session-uuid))
      (request
        (concat aaii-endpoint "generate_session")
        :type "POST"
        :headers '(("Content-Type" . "application/json"))
        :data (json-encode `(("session_uuid" . ,session-uuid)
                             ("model" . "gemini-1.5-flash")
                             ("instruction" . ,(cdr (assoc field aaii--model-instruction)))
                             ))
        :error (cl-function
                (lambda (&key _ &allow-other-keys)
                  (message "Build new session error.")
                  (setq aaii-session (delq (assoc field aaii-session) aaii-session))))
        :sync t))))

(defun aaii-region-request (beg end)
  (interactive "r")
  (aaii--build-session "code")
  (let ((prompt (concat "解释下面这段代码，并使用 markdown 格式化输出：\n" (buffer-substring-no-properties beg end))))
    (with-current-buffer (get-buffer-create "AAII")
      (goto-char (point-min))
      (insert (concat "# 询问\n" prompt "\n")))
    (request
      (concat aaii-endpoint "generate_text_stream")
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode `(("session_uuid" . ,(cdr (assoc "code" aaii-session)))
                           ("prompt" . ,prompt)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer (get-buffer-create "AAII")
                    (goto-char (point-min))
                    (insert (concat "# AAII 回答\n" data "\n"))
                    (switch-to-buffer-other-window (current-buffer))
                    (markdown-mode)
                    (goto-char (point-min))))))))

(defun aaii-code-ask (question)
  (interactive "sAsk:")
  (aaii--build-session "code")
  (let ((prompt question))
    (with-current-buffer (get-buffer-create "AAII")
      (goto-char (point-min))
      (insert (concat "# 询问\n" prompt "\n")))
    (request
      (concat aaii-endpoint "generate_text_stream")
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode `(("session_uuid" . ,(cdr (assoc "code" aaii-session)))
                           ("prompt" . ,prompt)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer (get-buffer-create "AAII")
                    (goto-char (point-min))
                    (insert (concat "# AAII 回答\n" data "\n"))
                    (switch-to-buffer-other-window (current-buffer))
                    (markdown-mode)
                    (goto-char (point-min))))))))

(defun aaii-lang-word-ask ()
  (interactive)
  (aaii--build-session "lang")
  (let ((prompt (concat "这个词语 " (thing-at-point 'word 'no-properties) " 是什么意思？")))
    (with-current-buffer (get-buffer-create "AAII")
      (goto-char (point-min))
      (insert (concat "# 询问\n" prompt "\n")))
    (request
      (concat aaii-endpoint "generate_text_stream")
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode `(("session_uuid" . ,(cdr (assoc "lang" aaii-session)))
                           ("prompt" . ,prompt)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer (get-buffer-create "AAII")
                    (goto-char (point-min))
                    (insert (concat "# AAII 回答\n" data "\n"))
                    (switch-to-buffer-other-window (current-buffer))
                    (markdown-mode)
                    (goto-char (point-min))))))))

(map! :leader
      :desc "aaii-region-request" "l r" #'aaii-region-request
      :desc "aaii-code-ask" "l a" #'aaii-code-ask
      :desc "aaii-lang-ask-word" "l w" #'aaii-lang-word-ask
      )
