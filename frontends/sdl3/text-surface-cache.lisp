(defpackage :lem-sdl3/text-surface-cache
  (:use :cl)
  (:export :clear-text-surface-cache
           :get-text-surface-cache
           :register-text-surface-cache
           :get-or-create-texture
           :clear-texture-cache
           :purge-stale-textures
           :sweep-if-oversize))
(in-package :lem-sdl3/text-surface-cache)

(defvar *text-surface-cache* (make-hash-table :test 'equal))

;;; Texture cache: maps surface wrapper object (eq identity) -> GPU texture.
;;; All drawing-object surfaces should be cached in *text-surface-cache*, making
;;; their Lisp identity stable across frames. The texture cache maps those stable
;;; surface objects to GPU textures via EQ lookup.
(defvar *texture-cache* (make-hash-table :test 'eq))

(defparameter *max-surface-cache-entries* 2000
  "When the surface cache exceeds this many keys, a full reset is triggered.
This bounds memory during sustained large-text output (e.g. cat huge-file).
The hot entries repopulate naturally on the next frame.")

(defparameter *max-texture-cache-entries* 3000
  "Safety threshold for *texture-cache*. If the texture cache grows beyond this
while the surface cache is within limits, orphaned textures are purged.
This is slightly larger than *max-surface-cache-entries* to allow for the
natural 1:N relationship (multiple surfaces may share a texture cache entry
during transitional frames).")

(defstruct cache-entry
  type
  attribute
  surface)

(defun clear-texture-cache ()
  "Destroy all cached GPU textures and clear the cache."
  (maphash (lambda (key texture)
             (declare (ignore key))
             (handler-case (lem-sdl3/ffi:destroy-texture texture)
               (error () nil)))
           *texture-cache*)
  (clrhash *texture-cache*))

(defun free-all-surfaces ()
  "Explicitly free all SDL surfaces in the surface cache."
  (maphash (lambda (key entries)
             (declare (ignore key))
             (dolist (entry entries)
               (let ((surface (cache-entry-surface entry)))
                 (when surface
                   (handler-case (lem-sdl3/ffi:destroy-surface surface)
                     (error () nil))))))
           *text-surface-cache*))

(defun clear-text-surface-cache ()
  (clear-texture-cache)
  (free-all-surfaces)
  (clrhash *text-surface-cache*))

(defun get-or-create-texture (renderer surface)
  "Return a cached GPU texture for SURFACE, creating one if needed.
Uses EQ identity on the surface wrapper object as cache key."
  (or (gethash surface *texture-cache*)
      (let ((texture (lem-sdl3/ffi:create-texture-from-surface renderer surface)))
        (setf (gethash surface *texture-cache*) texture)
        texture)))

(defun get-text-surface-cache (string attribute type)
  (dolist (entry (gethash string *text-surface-cache*))
    (when (and (lem-core:attribute-equal attribute (cache-entry-attribute entry))
               (eq type (cache-entry-type entry)))
      (return (cache-entry-surface entry)))))

(defun register-text-surface-cache (string attribute type surface)
  (push (make-cache-entry
         :type type
         :attribute attribute
         :surface surface)
        (gethash string *text-surface-cache*)))

(defun collect-live-surfaces ()
  "Return a hash-set (EQ hash table) of all surfaces currently stored
in *text-surface-cache*. These are the only surfaces whose textures
should be kept in *texture-cache*."
  (let ((live (make-hash-table :test 'eq)))
    (maphash (lambda (key entries)
               (declare (ignore key))
               (dolist (entry entries)
                 (setf (gethash (cache-entry-surface entry) live) t)))
             *text-surface-cache*)
    live))

(defun purge-stale-textures ()
  "Remove and destroy texture cache entries whose surface keys are not
in *text-surface-cache*. These are orphaned GPU textures from surfaces
that were evicted or replaced. Returns the number of textures purged."
  (let ((live (collect-live-surfaces))
        (stale-keys '()))
    ;; Collect stale keys first to avoid modifying the table during iteration
    (maphash (lambda (surface texture)
               (declare (ignore texture))
               (unless (gethash surface live)
                 (push surface stale-keys)))
             *texture-cache*)
    ;; Destroy and remove stale entries
    (dolist (surface stale-keys)
      (let ((texture (gethash surface *texture-cache*)))
        (when texture
          (handler-case (lem-sdl3/ffi:destroy-texture texture)
            (error () nil)))
        (remhash surface *texture-cache*)))
    (length stale-keys)))

(defun sweep-if-oversize ()
  "Bound cache memory. Runs each frame inside with-renderer.

Three-tier strategy:
1. If *text-surface-cache* exceeds *max-surface-cache-entries*: full reset
   of both caches + schedule :resize to invalidate drawing-caches.
2. Else if *texture-cache* exceeds *max-texture-cache-entries*: purge only
   orphaned textures (surfaces no longer in the surface cache).
3. Otherwise: no-op (O(1) check)."
  (cond
    ;; Tier 1: surface cache overflow → full reset
    ((> (hash-table-count *text-surface-cache*) *max-surface-cache-entries*)
     (clear-text-surface-cache)
     ;; Surfaces are freed explicitly above; drawing objects that still
     ;; reference them re-render on the next frame (see the :around method
     ;; on get-surface in drawing.lisp).
     (lem:send-event :resize))
    ;; Tier 2: texture cache overflow → purge orphaned textures only
    ((> (hash-table-count *texture-cache*) *max-texture-cache-entries*)
     (purge-stale-textures))))
