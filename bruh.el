;;; bruh.el --- BRowseUrlHelpers                     -*- lexical-binding: t; -*-

;; Homepage: https://github.com/a13/bruh
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; Author: Dmitry K. @a13
;; Keywords: hypertext, hypermedia, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for browse-url package

;;; Code:

(require 'browse-url)

(defgroup bruh nil
  "Extensions for browser-url"
  :prefix "bruh-"
  :link '(emacs-commentary-link "bruh")
  :group 'browse-url)

;;;###autoload
(defun bruh-chromium-new-app (url &optional _new-window)
  "Ask the Chromium WWW browser to load URL in app mode.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; TODO: add protocol if there's none
  (setq url (browse-url-encode-url url))
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "chromium " url) nil
	   browse-url-chromium-program
           (append
            browse-url-chromium-arguments
            (list (concat "--app=" url))))))

;;;###autoload
(defun bruh-feh (url &optional _ignore)
  "Browse image from URL using feh."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((process-environment (browse-url-process-environment)))
    ;; TODO: make feh path customizable
    (start-process (concat "feh " url) nil "feh" url)))

;;;###autoload
(defun bruh-mpv (url &optional _ignore)
  "Browse video on URL using mpv."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((process-environment (browse-url-process-environment)))
    ;; TODO: make mpv path customizable
    (start-process (concat "mpv " url) nil "mpv" "--loop-file=inf" url)))



(defcustom bruh-images-re
  '("\\.\\(jpe?g\\|png\\)\\(:large\\|:orig\\)?\\(\\?.*\\)?$")
  "Image URLs regular expressions list."
  :type (repeat 'string)
  :group 'bruh)

(defcustom bruh-images-browser-function #'eww-browse-url
  "Function to open image URL.
Set it to nil if you don't want to open images separately."
  :type 'function
  :group 'bruh)

(defun bruh-images-re-alist ()
  "Generate images browsing part for `browse-url-browser-function'."
  (if bruh-images-browser-function
      (mapcar (lambda (re)
                (cons re bruh-images-browser-function))
              bruh-images-re)
    nil))


(defcustom bruh-videos-re
  '("\\.\\(gifv?\\|avi\\|AVI\\|mp[4g]\\|MP4\\|webm\\)$"
    "^https?://\\(www\\.youtube\\.com\\|youtu\\.be\\|coub\\.com\\|vimeo\\.com\\|www\\.liveleak\\.com\\)/"
    "^https?://www\\.facebook\\.com/.*/videos?/")
  "Image URLs regular expressions list."
  :type (repeat 'string)
  :group 'bruh)

(defcustom bruh-videos-browser-function #'bruh-mpv
  "Function to open video URL.
Set it to nil if you don't want to open images separately."
  :type 'function
  :group 'bruh)

(defun bruh-videos-re-alist ()
  "Generate videos browsing part for `browse-url-browser-function'."
  (if bruh-videos-browser-function
      (mapcar (lambda (re)
                (cons re bruh-videos-browser-function))
              bruh-videos-re)
    nil))



(provide 'bruh)
;;; bruh.el ends here
