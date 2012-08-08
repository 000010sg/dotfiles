;;; ti-utils.el --- Utility functions related to TI

;; Author: Gareth Jones <gjones@drwholdings.com>
;; Maintainer: Gareth Jones <gjones@drwholdings.com>
;; Created: 02 Apr 2012
;; Version: 0.0.2

;;; Commentary:

;; Simply add this file to your load-path and require it in you
;; init.el:
;;
;; (require 'ti-utils)

;;; Code:

;;; -*- lexical-binding: t -*-

;; position management utils

(defun ti-utils/buffer->json (buffer)
  (with-current-buffer buffer
    (url-http-parse-response)
    (goto-char url-http-end-of-headers)
    (let ((json-string      (buffer-substring (point) (point-max)))
          (json-key-type    'symbol)
          (json-object-type 'alist))
      (json-read-from-string json-string))))

(defun ti-utils/get-json (url)
  (let* ((url-request-method "GET"))
    (ti-utils/buffer->json (url-retrieve-synchronously url))))

(defvar ti-utils-taps-servers
  '(hop
    clj-rrf
    eventproxy_rrf
    eventproxy
    routingserver
    createserver
    updateserver
    tradepersister))

(defun ti-utils/ti-config-for (environment)
  (let* ((ti-config (ti-utils/get-json (concat "http://ti/config/" environment "/api/applications")))
         (result (list)))
    (dolist (records ti-config)
      (let* ((app-name (car records))
             (config (cdr records)))
        (if (memq app-name ti-utils-taps-servers)
            (dolist (servers config)
              (let* ((server-name (car servers))
                     (server-config (cdr servers)))
                (dolist (instances server-config)
                  (let* ((instance-name (car instances))
                         (instance-config (cdr instances))
                         (log-config (cdr (assoc 'logs instance-config)))
                         (log-paths (mapcar (lambda (log) (cdr log)) log-config)))
                    (setq result (cons (list app-name (apply #'list (symbol-name server-name) log-paths))
                                       result)))
                  (setq instances (cdr instances))))
              (setq servers (cdr servers))))
        (setq records (cdr records))))
    result))

(defun ti-utils/grep-for (tstid date-suffix host &rest files)
  (mapc (lambda (file)
          (let* ((filename (concat file date-suffix))
                 (result (shell-command-to-string
                          (format "ssh prodappadmin@%s 'grep \"%s\" %s'" host tstid filename))))
            (princ (format "%s\n\n" filename))
            (if (string= "" result)
                (princ "no matches\n")
              (princ result))
            (princ "\n")
            (redisplay t)))
        files)
  (princ "-------------------------------------\n\n"))

;;;###autoload
(defun ti-utils/trace-trade-by-tstid (tstid days-back environment)
  (interactive "ststid: \nndays-back: \nsenv: ")
  (let* ((days-back (or days-back 0))
         (environment (if (string= environment "")
                          "sbs"
                        (if (string= environment "prod")
                            "production"
                          environment)))
         (date-suffix (if (> days-back 0)
                          (concat "."
                                  (format-time-string "%Y-%m-%d"
                                                      (time-add (current-time) (days-to-time (* -1 days-back)))))
                        "")))
    (message "environment: %s" environment)
    (with-output-to-temp-buffer "*ti-utils*"
      (pop-to-buffer "*ti-utils*")
      (princ (format "searching for %s\n\n" tstid))
      (let* ((ti-config (ti-utils/ti-config-for environment)))

        (dolist (server ti-utils-taps-servers)
          (dolist (config ti-config)
            (let* ((server-name (car config))
                   (opts        (cadr config)))
              (if (eq server-name server)
                  (apply #'ti-utils/grep-for tstid date-suffix opts)))))))
    nil))

;; clojure project management goodies

(defcustom ti-utils-src-dir "/src/git"
  "Directory in which all projects are kept"
  :group 'ti-utils
  :type  'string)

(defun ti-utils/print (msg)
  (princ (format "=> %s\n" msg))
  (redisplay t))

(defun ti-utils/find-project-files-cmd ()
  (concat "find " ti-utils-src-dir " -name project.clj"))

(defun ti-utils/all-project-files ()
  (let ((lines (shell-command-to-string (ti-utils/find-project-files-cmd))))
    (split-string lines "\n" t)))

(defun ti-utils/run-lein-deps-for (project-file)
  (let ((project-dir (file-name-directory project-file)))
    (ti-utils/print (format "running lein deps for %s" project-file))
    (ti-utils/print (shell-command-to-string (format "cd %s && lein deps" project-dir)))))

(defun ti-utils/git-commit-change (project-file lib version)
  (ti-utils/print (format "committing changes to %s to git" project-file))
  (let* ((project-dir (file-name-directory project-file))
         (whoami      (first (split-string (shell-command-to-string "whoami") "\n" t)))
         (commit-msg  (format "%s - upgrading %s to %s" whoami lib version)))
    (ti-utils/print (shell-command-to-string
                     (format "cd %s && git add %s && git commit -m '%s'"
                             project-dir
                             project-file
                             commit-msg)))))

(defun ti-utils/process-project (file-path lib version commit-p)
  (let ((file-changed-p nil)
        (lib-regexp     (concat "\\\(" lib " *\"\\\)\\\([^\"]+?\\\)\\\(\"\\\)")))
    (ti-utils/print (format "looking for %s in %s..." lib file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (while (re-search-forward lib-regexp nil t)
        (setq file-changed-p t)
        (replace-match (concat "\\1" version "\\3") nil nil))
      (if file-changed-p
          (progn
            (ti-utils/print (format "updated %s to %s!" lib version))
            (write-region (point-min) (point-max) file-path)
            (ti-utils/run-lein-deps-for file-path)
            (when commit-p
              (ti-utils/git-commit-change file-path lib version)))
        (ti-utils/print (format "not found, skipping"))))))

;;;###autoload
(defun ti-utils/update-clojure-dep (lib version commit-to-git)
  "Given a library name and a version, will find all project.clj
files below ti-utils-src-dir, search them for a dependency on
that library and if found, will update the dependency. `lein
deps` will be ran inside the directory automatically.

If commit-to-git is 'y', then the project.clj file will be
committed to the git repo with an appropriate message."
  (interactive "slib: \nsversion: \nscommit to git?(y/n): ")
  (let ((commit-p (string-equal "y" commit-to-git)))
    (with-output-to-temp-buffer "*ti-utils*"
      (pop-to-buffer "*ti-utils*")
      (mapc (lambda (f)
              (ti-utils/process-project f lib version commit-p))
            (ti-utils/all-project-files)))))

(provide 'ti-utils)

;;; ti-utils.el ends here
