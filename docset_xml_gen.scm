#!/usr/bin/env guile
!#

(use-modules (sxml simple))

(define title (car (command-line)))
(define args (cdr (command-line)))

; Initialize globals
(define docset-path        "")
(define plist-path         "")
(define out-path           "")
(define doc-name           "")
(define doc-other-versions "")
(define doc-url            "")
(define doc-version        "")

; Define help message
(define (print-help)
  (display (string-append "Usage: " title " [OPTION]... DOCSET\n"))
  (display                "Generate XML file for importing docsets into Dash or Zeal\n")
  (display (string-append "Example: " title " -o Ncurses.xml Ncurses.docset\n"))
  (display                "\n")
  (display                "Options:\n")
  (display                "  -h, --help                      Print this help message and exit\n")
  (display                "  -n, --name NAME                 Set the value of the \"name\" tag.\n")
  (display                "                                  Automatically determined if not set manually\n")
  (display                "      --other-versions VERSION    Add additional versions under the \"other-versions\" tag.\n")
  (display                "                                  Multiple values may be set using comma-separation\n")
  (display                "  -o, --out FILE                  Output to FILE instead of stdout\n")
  (display                "  -u, --url URL                   Set the value of the \"url\" tag.\n")
  (display                "                                  Multiple values may be set using comma-separation.\n")
  (display                "                                  Set to the local path if not set manually\n")
  (display                "  -v, --version VERSION           Set the value of the \"version\" tag.\n")
  (display                "                                  Automatically determined if not set manually\n"))

; Check for option
(define (check-options arg option-list)
  (cond
	((null? option-list) #f)
	((equal? arg (car option-list)) #t)
	(else
	  (check-options arg (cdr option-list)))))

; Read through options
; TODO split args for certain flags on ','
(define (read-options args)
  (cond
	((null? args)
	 (begin
	   (display "Error: missing DOCSET argument\n")
	   (exit)))
	((check-options (car args) '("-h" "--help"))
	 (begin
	   (print-help)
	   (exit)))
	((check-options (car args) '("-n" "--name"))
	 (begin
	   (set! doc-name (cadr args))
	   (read-options (cddr args))))
	((check-options (car args) '("--other-versions"))
	 (begin
	   (set! doc-other-versions (string-split (cadr args) #\,))
	   (read-options (cddr args))))
	((check-options (car args) '("-o" "--out"))
	 (begin
	   (set! out-path (cadr args))
	   (read-options (cddr args))))
	((check-options (car args) '("-u" "--url"))
	 (begin
	   (set! doc-url (string-split (cadr args) #\,))
	   (read-options (cddr args))))
	((check-options (car args) '("-v" "--version"))
	 (begin
	   (set! doc-version (cadr args))
	   (read-options (cddr args))))
	((check-options (car args) '("--"))
	 (set! docset-path (cadr args)))
	(else
	  (set! docset-path (car args)))))

(read-options args)

; Check validity of docset-path
(if (not (access? docset-path R_OK)) 
  (begin
	(display (string-append "Error: " docset-path " is not a valid path\n"))
	(exit)))
(if (not (equal? (stat:type (stat docset-path)) (string->symbol "directory")))
  (begin
	(display (string-append "Error: " docset-path " must be a directory\n"))
	(exit)))

; Attempt to locate plist-path
(set! plist-path (string-append docset-path "/Contents/Info.plist"))
(if (or (not (access? plist-path R_OK)) (not (equal? (stat:type (stat plist-path)) (string->symbol "regular"))))
  (begin
	(display (string-append "Error: " docset-path " is not a valid docset (could not find " plist-path ")\n"))
	(exit)))

; Parse plist for metadata
(define plist-port (open-file plist-path "r"))
(define plist-sxml (xml->sxml plist-port))
(close-port plist-port)

; SXML parser helper
; TODO cleanup and find a better way to parse SXML
(define (sxml-get-node sxml node)
  (if (and (not (null? sxml)) (list? sxml))
	(begin
	  (if (equal? (car sxml) (string->symbol node))
		(set! plist-sxml (cddr sxml)))
		(begin
		  (sxml-get-node (car sxml) node)
		  (sxml-get-node (cdr sxml) node)))))

; Compress plist-sxml to only keys and values
(sxml-get-node plist-sxml "dict")
(display plist-sxml)
(newline)
(display (cadadr (cdr plist-sxml)))
(newline)
