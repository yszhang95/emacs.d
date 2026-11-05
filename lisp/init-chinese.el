;;; init-chinese.el --- Configure Chinese input -*- lexical-binding: t -*-
;;; Commentary: enable Chinese input under org-mode
;;; Code:

(require-package 'pyim)
(require-package 'pyim-basedict)

;;; Chinese input from https://github.com/tumashu/pyim
(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-cregexp-utils)
(pyim-basedict-enable)

;;; put pyim-tsinghua-dict under ~/.emacs.d/site-lisp/
;;; see https://github.com/redguardtoo/pyim-tsinghua-dict
;;; modify the original pyim-tsinghua-dict-enable
(defun pyim-tsinghua-dict-enable-copy ()
  "Add tsinghua dict to pyim."
  (interactive)
  (let* ((dir (substitute-in-file-name "$HOME/.emacs.d/site-lisp/pyim-tsinghua-dict/"))
         (file (concat dir "pyim-tsinghua-dict.pyim")))
    (when (file-exists-p file)
      (if (featurep 'pyim-dict)
          (pyim-extra-dicts-add-dict
           `(:name "tsinghua-dict-elpa" :file ,file :elpa t))
        (message "pyim 没有安装，pyim-tsinghua-dict 启用失败。")))))

(pyim-tsinghua-dict-enable-copy)

;;; I do not want it to work globally
;;; I only write text in Chinese in org-mode.
;;; `default-input-method' only tells C-\ which method to use; nothing is
;;; activated globally.  M-j in org-mode activates pyim by itself.
(setq default-input-method "pyim")

(with-eval-after-load 'org
  ;; (define-key org-mode-map (kbd "C-M-j") #'pyim-toggle-input-ascii)
  (define-key org-mode-map (kbd "M-j") #'pyim-convert-string-at-point)
  (define-key org-mode-map (kbd "C-M-j")   #'pyim-convert-string-at-point))

;;; M-j in eat: turn the pinyin just typed into Chinese.
;;; `pyim-convert-string-at-point' cannot work there, because the program
;;; in the terminal owns the text.  So remember the letters sent to the
;;; terminal, pick the words in the minibuffer, then send one DEL per
;;; letter followed by the result.  Reading from the screen is avoided on
;;; purpose: TUIs such as Claude Code do not keep the cursor at the text.
(defconst my/eat-pyim-max-length 64
  "Forget the tracked letters when they grow longer than this.")

(defvar-local my/eat-pyim-pinyin ""
  "Lowercase letters most recently sent to the terminal in this buffer.")

(defvar my/eat-pyim--pending nil
  "Pinyin being converted by `my/eat-pyim--minibuffer-run'.")

(defun my/eat-pyim--track (n &optional e)
  "Update `my/eat-pyim-pinyin' for event E sent N times."
  (setq my/eat-pyim-pinyin
        (cond
         ((not (eql n 1)) "")
         ((and (characterp e) (or (<= ?a e ?z) (eql e ?')))
          (let ((pinyin (concat my/eat-pyim-pinyin (string e))))
            (if (> (length pinyin) my/eat-pyim-max-length) "" pinyin)))
         ((memq e '(127 backspace))
          (if (string-empty-p my/eat-pyim-pinyin)
              ""
            (substring my/eat-pyim-pinyin 0 -1)))
         (t ""))))

(defun my/eat-pyim--minibuffer-run ()
  "Run pyim on `my/eat-pyim--pending', then exit the minibuffer with the result."
  (interactive)
  (activate-input-method "pyim")
  (unwind-protect
      (progn
        (pyim-add-unread-command-events my/eat-pyim--pending)
        ;; Skip the probes, as `pyim-convert-string-at-point' does.
        (pyim-process--force-input-chinese)
        (insert (concat (pyim-input-method nil))))
    (deactivate-input-method))
  (exit-minibuffer))

(defun my/eat-pyim--read (pinyin)
  "Choose Chinese words for PINYIN; return \"\" when cancelled."
  (let ((my/eat-pyim--pending pinyin)
        (map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [my-eat-pyim-start] #'my/eat-pyim--minibuffer-run)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq unread-command-events
                (cons 'my-eat-pyim-start unread-command-events)))
      (read-from-minibuffer "pyim: " nil map))))

(defun my/eat-pyim-convert ()
  "Replace the pinyin just typed in the terminal with Chinese.
Without tracked pinyin, send M-j to the terminal as usual."
  (interactive)
  (let ((pinyin my/eat-pyim-pinyin))
    (if (string-empty-p pinyin)
        (eat-self-input 1 (aref (kbd "M-j") 0))
      (let ((result (my/eat-pyim--read pinyin)))
        ;; Cancelling leaves the letters on screen, so keep tracking them.
        (unless (or (string-empty-p result) (string= result pinyin))
          (eat-term-send-string
           eat-terminal
           (concat (make-string (length pinyin) ?\d) result))
          (setq my/eat-pyim-pinyin ""))))))

;; The probes look at the text before point, which in a terminal is not
;; what is being typed: with them, C-\ in eat would only produce English.
(defun my/eat-pyim-disable-probes ()
  "Let pyim input Chinese in this eat buffer whenever it is active."
  (setq-local pyim-english-input-switch-functions nil))

(add-hook 'eat-mode-hook #'my/eat-pyim-disable-probes)

(with-eval-after-load 'eat
  (advice-add 'eat-self-input :before #'my/eat-pyim--track)
  (define-key eat-semi-char-mode-map (kbd "M-j") #'my/eat-pyim-convert))

;; 显示5个候选词。
(setq pyim-page-length 5)

;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
;; (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
;; (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

;; 我使用全拼
(pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'wubi)
;; (pyim-default-scheme 'cangjie)

;; 我使用云拼音
(setq pyim-cloudim 'google)

;; pyim 探针设置
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))
;; (setq-default pyim-punctuation-translate-p '(no))

;; 开启代码搜索中文功能（比如拼音，五笔码等）
;; (pyim-isearch-mode 1)
(add-hook 'org-mode-hook (lambda () (pyim-isearch-mode 1)))


(provide 'init-chinese)
;;; init-chinese.el ends here
