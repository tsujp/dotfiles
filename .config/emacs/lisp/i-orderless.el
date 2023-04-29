;;; -*- lexical-binding: t -*-

;;; Character-order-independent completions.

;;; docs: https://github.com/oantolin/orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (sty
                                          les basic partial-completion)))))

(provide 'i-orderless)
