;;; nix-store.el -- run nix commands in Emacs -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix
;; Version: 1.4.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'nix)
(require 'magit-section)

(defgroup nix-store nil
  "nix-store customizations"
  :group 'nix)

(defun nix-store-realise (path)
  "Realise a path asynchronously.
PATH the path within /nix/store to realise"
  (make-process
   :buffer nil
   :command (list nix-store-executable "--realise" path)
   :noquery t
   :name (format "*nix-store*<%s>" path)))

(defvar-local nix-buffer-store-path nil "Buffer-local object holding an `nix-store-path` object")

(defclass nix-store-path ()
  ((path :initarg :path :accessor nix-store-path-path)
    (status :initarg :status :initform 'realised)
    (hash :initarg :hash)
    (size :initarg :size)
    (derivers :initarg :derivers)
    (outputs :initarg :outputs)
    (references :initarg :references)
    (referrers :initarg :referrers)
    (requisites :initarg :requisites))
  "Nix-Store-Path Class that holds all information of the path that
is displayed")

(cl-defmethod nix-store-fill-data ((object nix-store-path))
  (oset object :size (nix-store--query 'size (nix-store-path-path object)))
  (oset object :hash (nix-store--query 'hash (nix-store-path-path object)))
  (oset object :derivers (nix-store--query 'deriver (nix-store-path-path object)))
  (oset object :outputs (nix-store--query 'outputs (nix-store-path-path object)))
  (oset object :referrers (nix-store--query 'referrers (nix-store-path-path object)))
  (oset object :requisites (nix-store--query 'requisites (nix-store-path-path object)))
  (oset object :references (nix-store--query 'references (nix-store-path-path object)))
  object)

(cl-defun nix-store--query (argument &optional (path (nix-store-path-path nix-buffer-store-path)))
  (let ((nix-executable nix-store-executable))
    (cond
      ((eq 'deriver argument)
	;; Special treatment for 'derivers', we want to treat a single entry
	;; with this string as an empty list
	(remove "unknown-deriver"
	  (nix--process-lines "--query" "--deriver" path )))
      ((eq 'size argument) (string-to-number (nix--process-string "--query" "--size" path )))
      ((eq 'hash argument) (nix--process-string "--query" "--hash" path ))
      ((eq 'requisites argument) (nix--process-lines "--query" "--requisites" path ))
      ((eq 'references argument) (nix--process-lines "--query" "--references" path ))
      ((eq 'referrers argument) (nix--process-lines "--query" "--referrers" path ))
      ((eq 'outputs argument)
	(ignore-errors
	  ;; This can fail for non-derivation paths
	  (nix--process-lines "--query" "--outputs" path )))
      (t (error "Unknown argument to nix-store --query: %s" argument)))))

(cl-defun nix-store-path-insert-path (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the path."
  (magit-insert-section (path (oref store-path path))
    (magit-insert-heading (propertize (format "%-11s" "Path:") 'face 'magit-section-heading)
      (format "%s" (oref store-path path)))))

(cl-defun nix-store-path-insert-size (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the size."
  (magit-insert-section (size (oref store-path size))
    (magit-insert-heading (propertize (format "%-11s" "Size:") 'face 'magit-section-heading)
      (format "%s" (oref store-path size)))))

(cl-defun nix-store-path-insert-hash (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the hash."
  (magit-insert-section (hash (oref store-path hash))
    (magit-insert-heading (propertize (format "%-11s" "Hash:") 'face 'magit-section-heading)
      (format "%s" (oref store-path hash)))))

(cl-defun nix-store-path-insert-status (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the status."
  (magit-insert-section (status (oref store-path status))
    (magit-insert-heading (propertize (format "%-11s" "Status:") 'face 'magit-section-heading)
      (format "%s" (oref store-path status)))))

(defmacro nix-store--magit-insert-section-list (type value label)
  `(let ((value ,value))
     (when (and (listp value) (> (length value) 0))
       (magit-insert-section (,type value)
	 (magit-insert-heading ,label)
	 (cl-loop for x in value do (magit-insert-section (store-path x) (insert x) (newline)))
	 (insert ?\n)
	 (magit-insert-child-count (magit-current-section))))))

(cl-defun nix-store-path-insert-derivers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all derivers."
  (nix-store--magit-insert-section-list derivers (oref store-path derivers) "Derivers:"))

(cl-defun nix-store-path-insert-outputs (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all outputs."
  (nix-store--magit-insert-section-list outputs (oref store-path outputs) "Outputs:"))

(cl-defun nix-store-path-insert-references (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all references."
  (nix-store--magit-insert-section-list references (oref store-path references) "References:"))

(cl-defun nix-store-path-insert-referrers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all referrers."
  (nix-store--magit-insert-section-list referrers (oref store-path referrers) "Referrers:"))

(cl-defun nix-store-path-insert-requisites (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all requisites."
  (nix-store--magit-insert-section-list requisites (oref store-path requisites) "Requisites:"))

(defcustom nix-store-path-headers-hook
  '(nix-store-path-insert-path
     nix-store-path-insert-status
     nix-store-path-insert-hash
     nix-store-path-insert-size)
  "Hook run to insert headers into the nix-store buffer."
  :group 'nix-store
  :type 'hook
  :options '(nix-store-path-insert-path
	      nix-store-path-insert-status
	      nix-store-path-insert-hash
	      nix-store-path-insert-size))

(defcustom nix-store-path-sections-hook
  '(nix-store-path-insert-derivers
     nix-store-path-insert-outputs
     nix-store-path-insert-references
     nix-store-path-insert-referrers
     nix-store-path-insert-requisites)
  "Hook run to insert sections into a nix-store buffer."
  :group 'nix-store
  :type 'hook)

(defun nix-store-show-path (path)
  "Show a nix-store path.

If you want to change the order of the section lists ( or even
implement your own ones ) you can customize the variable
'nix-store-path-sections-hook'."
  (interactive "DNix-Store-Path: ")
  (setq path (expand-file-name path default-directory))
  (switch-to-buffer (format "Nix Store Path: %s" path))
  (nix-store-path-mode)
  (setq nix-buffer-store-path (nix-store-fill-data (make-instance 'nix-store-path :path path))
    list-buffers-directory path)
  ;; Maybe this should go at the end of the rendering
  (when (file-directory-p path)
    (setq default-directory path))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (store-path)
      (magit-insert-headers 'nix-store-path-headers-hook)
      (magit-run-section-hook 'nix-store-path-sections-hook))
    (setf (point) (point-min))))

(defun nix-store-path-at-point ()
  "Returns the nix-store path at point."
  ;; TODO extract this via magit-section values
  (substring-no-properties (thing-at-point 'filename)))

(defun nix-store-show-path-at-point ()
  "Opens the nix-store-path at point.

It uses \\[nix-store-show-path] to display the store path."
  (interactive)
  (nix-store-show-path (nix-store-path-at-point)))

(defvar nix-store-path-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nix-store-show-path-at-point)
    map))

(define-derived-mode nix-store-path-mode magit-section-mode "Nix Store Path"
  (read-only-mode 1))

(provide 'nix-store)
;;; nix-store.el ends here
