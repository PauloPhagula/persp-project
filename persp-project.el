;;; persp-project.el --- Perspective integration with built-in project

;; Copyright (C) 2024 Paulo Phagula

;; Author: Paulo Phagula
;; Created: 2024-07-07
;; Keywords: project, convenience
;; Version: 0.0.1
;; Package-Requires: ((perspective "1.9") (project "0.6.1") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
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

(defmacro persp-project-bridge (func-name)
  "Create advice to create a perspective before invoking function FUNC-NAME.
The advice provides a bridge between perspective and project
functions when switching between projects. After switching to a new
project, this advice creates a new perspective for that project."
  `(defadvice ,func-name (before project-create-perspective-after-switching-projects activate)
     "Create a dedicated perspective for the current project's window after switching projects."
     (let ((project-name (file-name-nondirectory (directory-file-name (project-root (project-current))))))
       (when (and persp-mode (project-current))
         (persp-switch project-name)))))

(persp-project-bridge project-dired)
(persp-project-bridge project-find-file)

;;;###autoload
(defun persp-project-switch-project (project-to-switch)
  "Switch to a project or perspective we have visited before.
If the perspective of the corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `PROJECT-SWITCH-PROJECT' invokes
`project-switch-project-action'.

Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective."
  (interactive (list (completing-read "Switch to project: " (project-known-project-roots))))
  (let* ((project-root (file-name-as-directory project-to-switch))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (persp (gethash project-name (perspectives-hash))))
    (cond
     ;; project-specific perspective already exists
     ((and persp (not (equal persp (persp-curr))))
      (persp-switch project-name))
     ;; persp exists but does not match with project-name
     ((and persp (not (equal persp project-name)))
      (persp-switch project-name)
      (project-switch-project project-root))
     ;; project-specific perspective doesn't exist
     ((not persp)
      (let ((frame (selected-frame)))
        (persp-switch project-name)
        (project-switch-project project-root)
        ;; Clean up if we switched to a new frame. `helm' for one allows finding
        ;; files in new frames so this is a real possibility.
        (when (not (equal frame (selected-frame)))
          (with-selected-frame frame
            (persp-kill project-name))))))))

(defadvice persp-init-frame (after persp-project-init-frame activate)
  "Rename initial perspective to `project-name' when a new frame is created in a known project."
  (with-selected-frame frame
    (when (project-current)
      (persp-rename (file-name-nondirectory (directory-file-name (project-root (project-current))))))))

(provide 'persp-project)
;;; persp-project.el ends here