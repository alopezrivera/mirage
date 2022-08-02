;; package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("org"   . "https://orgmode.org/elpa/")
			   ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; ensure
(setq use-package-always-ensure t)
