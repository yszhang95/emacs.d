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
;; (setq default-input-method "pyim")
(add-hook 'org-mode-hook
          (lambda () (pyim-activate) (pyim-toggle-input-ascii)))

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "M-j") 'pyim-toggle-input-ascii)))

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
;; (setq pyim-cloudim 'baidu)

;; pyim 探针设置
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))

;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))
(setq-default pyim-punctuation-translate-p '(no))

;; 开启代码搜索中文功能（比如拼音，五笔码等）
;; (pyim-isearch-mode 1)

(provide 'init-chinese)
;;; init-chinese.el ends here
