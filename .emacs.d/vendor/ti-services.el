;;; ti-services.el --- Functions for TI APIs

;; Author: Gareth Jones <gjones@drwholdings.com>
;; Maintainer: Gareth Jones <gjones@drwholdings.com>
;; Created: 02 Apr 2012
;; Version: 0.0.1

;;; Commentary:

;; Simply add this file to your load-path and require it in you
;; init.el. You can set ti-nerd-url, ti-ord-url, and ti-rds-url to
;; specific values or they will default to the mirror environment.

;;; Code:

;;; -*- lexical-binding: t -*-

(require 'url)
(require 'json)

(defgroup ti-services nil
  "interface to ti services api")

(defcustom ti-services-nerd-url "http://ti-dev/nerd-mirror/api/"
  "nerd url"
  :group 'ti-services
  :type 'string)

(defcustom ti-services-ord-url "http://ti-dev/controltower-mirror/api/"
  "ord url"
  :group 'ti-services
  :type 'string)

(defcustom ti-services-rds-url "http://rds/"
  "rds url"
  :group 'ti-services
  :type 'string)

;; util functions

(defun tis/use-mirror! ()
  (interactive)
  (setq ti-nerd-url "http://ti-dev/nerd-mirror/api/")
  (setq ti-ord-url "http://ti-dev/controltower-mirror/api/"))

(defun tis/use-sandbox! ()
  (interactive)
  (setq ti-nerd-url "http://ti-dev/nerd-sandbox/api/")
  (setq ti-ord-url "http://ti-dev/controltower-sandbox/api/"))

(defun tis/use-production! ()
  (interactive)
  (setq ti-nerd-url "http://ti/nerd/api/")
  (setq ti-ord-url "http://ti/controltower/api/"))

(defun tis/to-url (base &rest parts)
  (concat base (mapconcat 'identity parts "/")))

(defun tis/print (msg)
  (princ (format "%s\n" msg)))

(defun tis/with-params (url &rest params)
  (let (data)
    (while params
      (setq data (cons (concat (substring (symbol-name (car params)) 1)
                               "=" (url-hexify-string (cadr params)))
                       data))
      (setq params (cddr params)))
    (concat url "?" (mapconcat 'identity data "&"))))

(defun tis/buffer->json (buffer)
  (with-current-buffer buffer
    (url-http-parse-response)
    (goto-char url-http-end-of-headers)
    (let ((json-string      (buffer-substring (point) (point-max)))
          (json-key-type    'keyword)
          (json-object-type 'alist))
      (json-read-from-string json-string))))

(defun tis/get-json (url)
  (let* ((url-request-method "GET"))
    (tis/buffer->json (url-retrieve-synchronously url))))

(defun tis/show-json (data &optional filter-pred)
  (let* ((filter-pred (or filter-pred (lambda (x) t)))
         (json-data   (remove-if-not filter-pred data)))
    (with-output-to-temp-buffer "*ti-services*"
      (pop-to-buffer "*ti-services*")
      (tis/print (format "got %d result(s):\n" (length json-data)))
      (dolist (records (append json-data nil))
        (while records
          (let* ((record (car records))
                 (k (car record))
                 (v (cdr record)))
            (tis/print (format "%s => %s" k v))
            (setq records (cdr records))))
        (tis/print "")))))

(defun tis/maybe-append (l &rest kvp)
  (while kvp
    (let ((k (car kvp))
          (v (cadr kvp)))
      (unless (or (string= "" v)
                  (not v))
        (setq l (append l (list k v))))
      (setq kvp (cddr kvp))))
  l)

;; nerd functions

(defun tis/to-nerd-url (&rest parts)
  (apply #'tis/to-url ti-services-nerd-url parts))

(defun tis/instrument-by-rds-id (rds-id)
  (interactive "srds-id: ")
  (tis/show-json (tis/get-json
                  (tis/with-params (tis/to-nerd-url "instruments")
                                   :rds_id rds-id))))

(defun tis/instrument-by-short-symbol (short-symbol)
  (interactive "sshort-symbol: ")
  (tis/show-json (tis/get-json
                  (tis/with-params (tis/to-nerd-url "instruments")
                                   :rci_short_symbol short-symbol))))

(defun tis/instrument-by-long-symbol (long-symbol)
  (interactive "slong-symbol: ")
  (tis/show-json (tis/get-json
                  (tis/with-params (tis/to-nerd-url "instruments")
                                   :rci_long_symbol long-symbol))))

(defun tis/find-futures (product-symbol &optional year month)
  (interactive "sproduct-symbol: \nsyear: \nsmonth: ")
  (tis/show-json (tis/get-json
                  (apply #'tis/with-params (tis/to-nerd-url "futures" "search")
                         (tis/maybe-append (list :drw_product_symbol product-symbol)
                                           :listed_year year
                                           :listed_month month)))))

(defun tis/find-options (product-symbol year month &optional strike call-put)
  (interactive "sproduct-symbol: \nsyear: \nsmonth: \nsstrike: \nscall-put (C|P): ")
  (tis/show-json (tis/get-json
                  (apply #'tis/with-params (tis/to-nerd-url "options" "search")
                         (tis/maybe-append (list :drw_product_symbol product-symbol)
                                           :listed_year year
                                           :listed_month month
                                           :strike strike
                                           :call_put call-put)))))

(defun tis/find-currency-pairs (base counter)
  (interactive "sbase: \nscounter: ")
  (tis/show-json (tis/get-json
                  (apply #'tis/with-params (tis/to-nerd-url "currency_pairs" "search")
                         (tis/maybe-append '()
                                           :base_currency base
                                           :counter_currency counter)))))

(defun tis/find-bonds (product-symbol maturity-date coupon-rate)
  (interactive "sproduct-symbol: \nsmaturity-date: \nscoupon-rate: ")
  (tis/show-json (tis/get-json
                  (apply #'tis/with-params (tis/to-nerd-url "bonds" "search")
                         (tis/maybe-append '()
                                           :drw_product_symbol product-symbol
                                           :maturity_date maturity-date
                                           :coupon_rate coupon-rate)))))

(defun tis/find-cash-flows (name)
  (interactive "sname: ")
  (tis/show-json (tis/get-json
                  (tis/with-params (tis/to-nerd-url "cash_flows" "search")
                                   :name name))))

;; ord functions

(defun tis/to-ord-url (&rest parts)
  (apply #'tis/to-url ti-services-ord-url parts))

(defmacro tis/defun-ord-find (type)
  (let ((pred (make-symbol "pred")))
    `(defun ,(intern (concat "tis/find-"
                             (replace-regexp-in-string "_" "-" type)))
       (&optional name)
       (interactive "sname: ")
       (let ((,pred (if (string= "" name)
                        (lambda (x) t)
                      (lambda (d) (string-match name (cdr (assoc :name d)))))))
         (tis/show-json (tis/get-json (tis/to-ord-url ,(concat type ".json"))) ,pred)))))

(dolist (type '("trading_desks"
                "trading_groups"
                "clearing_accounts"
                "clearing_firms"))
  (eval `(tis/defun-ord-find ,type)))

;; rds functions

(defun tis/equity-by-rds-id (rds-id)
  (interactive "srds-id: ")
  (let* ((url (tis/with-params (tis/to-url ti-services-rds-url "equity" "issue")
                               :rdsIds rds-id))
         (data (tis/get-json url))
         (instruments (cdr (assoc :instruments data))))
    (tis/show-json instruments)))

(provide 'ti-services)

;;; ti-services.el ends here
