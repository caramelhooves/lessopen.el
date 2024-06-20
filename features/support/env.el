;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'lessopen)
  (require 'espuds)
  (require 'ert))
