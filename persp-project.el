;;; persp-project.el --- Perspective integration with built-in project  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Paulo Phagula
;;
;; Licensed under the MIT license.

;; Author: Paulo Phagula <paulo.phagula@gmail.com>
;; Maintainer: Paulo Phagula <paulo.phagula@gmail.com>
;; Created: 2024-07-07
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (perspective "1.9") (project "0.6.1"))
;; Keywords: project, workspace, convenience, frames
;; URL: https://github.com/PauloPhagula/persp-project
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This library bridges perspective mode to the built-in project library.
;; The idea is to create a separate perspective when switching projects.
;; A perspective is an independent workspace for Emacs, similar to multiple
;; desktops in Gnome and MacOS. This integration allows you to easily know
;; which project you're currently in, and focus on files that only belong
;; to the current project when switching buffers.

;; To use this library, put this file in your Emacs load path, and
;; call (require 'persp-project)

;; See perspective.el on github: https://github.com/nex3/perspective-el

;;; Code:
(require 'perspective)
(require 'project)

(defvar persp-project-mode nil)
(defgroup persp-project-bridge nil
  "persp-mode and project integration."
  :group 'persp-mode
  :group 'project
  :prefix "persp-project-bridge-"
  :link '(url-link :tag "Github" "https://github.com/PauloPhagula/persp-project"))

(defun persp-project--create-advice (func-name)
  "Create advice to create a perspective before invoking function FUNC-NAME.
The advice provides a bridge between perspective and project
functions when switching between projects. After switching to a new
project, this advice creates a new perspective for that project."
  (eval
   `(defadvice ,func-name (before persp-project--create-perspective-after-switching activate)
      "Create a dedicated perspective for the current project's window after switching projects."
      (let ((project-name (file-name-nondirectory (directory-file-name (project-root (project-current))))))
        (when (and persp-mode (project-current))
          (persp-switch project-name))))))

(defun persp-project--remove-advice (func-name)
  "Remove the advice for FUNC-NAME created by `persp-project--create-advice`."
  (eval
   `(ad-remove-advice ',func-name 'before 'persp-project--create-perspective-after-switching)
   `(ad-update ',func-name)))

(defun persp-project-switch-project (project-to-switch)
  "Switch to a project or perspective we have visited before.
If the perspective of the corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `project-switch-project' invokes
`project-switch-project-action`.

Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective.

PROJECT-TO-SWITCH denotes the project/perspective."
  (interactive (list (completing-read "Switch to project: " (project-known-project-roots))))
  (let* ((project-root (file-name-as-directory project-to-switch))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (persp (gethash project-name (perspectives-hash))))
    (cond
     ;; Project-specific perspective already exists
     ((and persp (not (equal persp (persp-curr))))
      (persp-switch project-name))
     ;; Perspective exists but does not match with project-name
     ((and persp (not (equal persp project-name)))
      (persp-switch project-name)
      (project-switch-project project-root))
     ;; Project-specific perspective doesn't exist
     ((not persp)
      (let ((frame (selected-frame)))
        (persp-switch project-name)
        (project-switch-project project-root)
        ;; Clean up if we switched to a new frame. `helm` for one allows finding
        ;; files in new frames so this is a real possibility.
        (when (not (equal frame (selected-frame)))
          (with-selected-frame frame
            (persp-kill project-name))))))))

(defadvice persp-init-frame (after persp-project--init-frame activate)
  "Rename initial perspective to `project-name` when a new frame is created in a known project."
  (with-selected-frame frame
    (when (project-current)
      (persp-rename (file-name-nondirectory (directory-file-name (project-root (project-current))))))))

;;;###autoload
(define-minor-mode persp-project-mode
  "`perspective-mode` and `project` integration.
Creates perspective for projects."
  :require 'persp-project
  :group 'persp-project
  :init-value nil
  :global t

  (if persp-project-mode
      (if (and persp-mode (featurep 'project))
          (progn
            (persp-project--create-advice 'project-dired)
            (persp-project--create-advice 'project-find-file)
            (ad-enable-advice 'persp-init-frame 'after 'persp-project--init-frame)
            (ad-activate 'persp-init-frame))
        (message "You cannot enable persp-project-mode unless persp-mode and project are active.")
        (setq persp-project-mode nil))

    ;; Disable advices when the mode is turned off
    (progn
      (persp-project--remove-advice 'project-dired)
      (persp-project--remove-advice 'project-find-file)
      (ad-disable-advice 'persp-init-frame 'after 'persp-project--init-frame)
      (ad-update 'persp-init-frame))))

(provide 'persp-project)
;;; persp-project.el ends here
