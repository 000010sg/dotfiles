;;; ti-utils.el --- Utility functions related to TI

;; Author: Gareth Jones <gjones@drwholdings.com>
;; Maintainer: Gareth Jones <gjones@drwholdings.com>
;; Created: 02 Apr 2012
;; Version: 0.0.1

;;; Commentary:

;; Simply add this file to your load-path and require it in you
;; init.el:
;;
;; (require 'ti-utils)

;;; Code:

;;; -*- lexical-binding: t -*-

;; position management utils

(defvar ti-utils-servers
  '(("sbs" . ((clj-rff        . ("sup-chiti20" "/sitelogs/clj-rrf/eventproxy/"
                                 "inbound.log" "main.log" "outbound.log"))
              (eventproxy     . ("sup-chiti20" "/sitelogs/eventproxy/"
                                 "inbound.log" "eventproxy.log" "outbound-rabbit.log"))
              (routingserver  . ("sup-chiti20" "/sitelogs/routingserver/"
                                 "inbound.log" "routingserver.log" "outbound.log"))
              (cs             . ("sup-chiti20" "/sitelogs/createserver/default/"
                                 "inbound.log" "main.log" "outbound-json.log"))
              (updateserver   . ("sup-chiti20" "/sitelogs/updateserver/"
                                 "inbound.log" "updateserver.log" "outbound.log"))
              (tradepersister . ("sup-chiti20" "/sitelogs/tradepersister/"
                                 "inbound.log" "tradepersister.log" "sql.log"))))
    ("prod" . ((eventproxy     . ("sup-chiti04" "/sitelogs/eventproxy/"
                                  "inbound.log" "eventproxy.log" "outbound-rabbit.log"))
               (routingserver  . ("sup-chiti04" "/sitelogs/routingserver/"
                                  "inbound.log" "routingserver.log" "outbound.log"))
               (cs             . ("sup-chiti04" "/sitelogs/createserver/*/"
                                  "inbound.log" "main.log" "outbound-json.log"))
               (updateserver   . ("sup-chiti04" "/sitelogs/updateserver/"
                                  "inbound.log" "updateserver.log" "outbound.log"))))))

(defun ti-utils/grep-for (tstid date-suffix host dir &rest files)
  (princ (format "%s\n-------------------------------------\n" dir))
  (mapc (lambda (file)
          (let* ((filename (concat file date-suffix))
                 (result (shell-command-to-string
                          (format "ssh prodappadmin@%s 'grep \"%s\" %s/%s'" host tstid dir filename))))
            (princ (format "%s\n" filename))
            (if (string= "" result)
                (princ "No matches\n")
              (princ result))
            (princ "\n")
            (redisplay t)))
        files))

;;;###autoload
(defun ti-utils/trace-trade-by-tstid (tstid days-back environment)
  (interactive "ststid: \nndays-back: \nsenv: ")
  (let* ((days-back (or days-back 0))
         (environment (if (string= environment "") "sbs" environment))
         (date-suffix (if (> days-back 0)
                          (concat "."
                                  (format-time-string "%Y-%m-%d"
                                                      (time-add (current-time) (days-to-time (* -1 days-back)))))
                        "")))
    (message "environment: %s" environment)
    (with-output-to-temp-buffer "*ti-utils*"
      (pop-to-buffer "*ti-utils*")
      (princ (format "searching for %s\n\n" tstid))
      (mapc (lambda (s)
              (let* ((server (car s))
                     (opts   (cdr s)))
                (apply #'ti-utils/grep-for tstid date-suffix opts)))
            (cdr (assoc-string environment ti-utils-servers))))))

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
