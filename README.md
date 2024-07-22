[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
<!-- [![MELPA](http://melpa.org/packages/persp-project-badge.svg)](http://melpa.org/#/persp-project)
[![MELPA Stable](http://stable.melpa.org/packages/persp-project-badge.svg)](http://stable.melpa.org/#/persp-project) -->

## Introduction

[Perspective](https://github.com/nex3/perspective-el) is a minor mode
that provides the ability to manage different workspaces. If you need
to open many projects at the same time, perspective can help you keep
each project related buffers and windows setting separate from other
projects, similar to multiple spaces on MacOS, which allows you to
focus on the files of the current active project.

This library bridges perspective mode to the built-in project library.
The idea is to create a separate perspective when switching projects.
A perspective is an independent workspace for Emacs, similar to multiple
desktops in Gnome and MacOS. This integration allows you to easily know
which project you're currently in, and focus on files that only belong
to the current project when switching buffers.

Demo:

[![Demo](https://img.youtube.com/vi/CkiKLR7B634/0.jpg)](https://youtube.com/watch?v=CkiKLR7B634)

> [!NOTE]  
> This package has come to life adapting the great package [persp-projectile by Bozhidar Batsov](https://github.com/bbatsov/persp-projectile) with ChatGPT.
> All credit goes to him ... I have been using his package for years now.
> What inspired me to create this package is my recent effort to avoid most third party packages, specially if they prefer a newly defined interface over the built-in Emacs interface

## Installation

I'll assume that if you want to use this package you're already using perspective and have set it up already.
If you're not, you can find the install instructions in the project's README.

This package is not yet available on MELPA, so you can either install it manually or by combining use-package with straight.el or quelpa.

## Manually

Download the `persp-project.el` file and add it to your load path.
Most likely you will want to add it to your `~/.emacs.d/lisp` directory.

And then request it via use-package more or less like so:

```elisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package persp-project
  :load-path "lisp/persp-project"
  :after (perspective project))
```

## Automatically through straight.el or quelpa

If you're using straight.el, you can add the following to your init.el:

```elisp
(use-package persp-project
  :straight (persp-project :type git :host github :repo "PauloPhagula/persp-project")
  :after (perspective project))
```

## Known issues

Check out the project's
[issue list](https://github.com/PauloPhagula/persp-project/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## Credits / Thanks

- Bozhidar Batsov ([bbatsov]((https://github.com/bbatsov)) and contributors of the original [persp-projectile](https://github.com/bbatsov/persp-projectile) package.
- Natalie Weizenbaum ([nex3](https://github.com/nex3)) and contributors of the original [perspective](https://github.com/nex3/perspective-el) package.
- ChatGTP for helping me adapt Bozhidar's code to use the Emacs built-in project.el.

## Contributors

Here's a [list](https://github.com/PauloPhagula/persp-project/contributors) of all the people who have contributed to the
development of persp-project.

## License

Copyright © 2024 Paulo Phagula and [contributors](https://github.com/PauloPhagula/persp-project/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
