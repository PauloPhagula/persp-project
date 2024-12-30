;;; persp-project-test.el --- Test suite for persp-project -*- lexical-binding: t -*-

;; Author: Paulo Phagula <paulo.phagula@gmail.com>
;; Created: 2024-12-30
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Regression tests for persp-project.

;;; Code:

(require 'ert)
(require 'persp-project)

(ert-deftest enable-mode-test ()
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  (should (persp-project-mode t)))

(ert-deftest errors-when-activating-whithout-enabling-deps-first-test ()
  (persp-mode 0)
  (should-not (persp-project-mode t)))

(provide 'persp-project-test)
