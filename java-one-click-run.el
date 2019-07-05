;;; java-one-click-run.el --- -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 James Borden, Mingde (Matthew) Zeng
;;
;; Filename: java-one-click-run.el
;; Description:
;; Author: James Borden, Mingde (Matthew) Zeng
;; Maintainer: Mingde (Matthew) Zeng
;; Created: Wed Jul  3 17:13:00 2019 (-0400)
;; Version: 0.0.1
;; Package-Requires: shell-here
;; Last-Updated: Thu Jul  4 21:57:25 2019 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This package compiles and runs the current .java file using the function `javac-compile-and-run'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; java-one-click-run originated from James Borden's javac-mode (https://github.com/jborden/emacs/blob/master/java/javac.el)
;; that was released under MIT license.
;;
;; Copyright (c) 2019 James Borden
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2019.07.04
;; - First release
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; There're number of modifications and enhancements to the original package,
;; it's now relicensed as GPLv3.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'shell-here))

(defun javac-compile-class ()
  "Compile a java file into a .class file."
  (interactive)
  (let ((javac-command (concat "javac -cp " default-directory " " buffer-file-name)))
    (shell-command javac-command)))

(defun javac-run-java ()
  "Run the the java file in the current project directory using `shell-here'."
  (interactive)
  (let* ((class-name (substring buffer-file-name (string-match "[^\/]*\.java$" buffer-file-name) -5))
         (javac-command (concat "java -cp " default-directory " " class-name)))
    (progn (shell-here)
           (comint-stop-subjob)
           (erase-buffer)
           (insert javac-command)
           (comint-send-input))))

(defun javac-compile-and-run ()
  "Compile, if successful, then run the current java file using `shell-here'."
  (interactive)
  (when (= (javac-compile-class) 0)
    (javac-run-java)))

(provide 'java-one-click-run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java-one-click-run.el ends here
