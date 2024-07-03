(Then "^I open \"\\([^\"]+\\)\" with lessopen$"
  (lambda (filename)
    (find-file-with-lessopen filename)))

(Then "^I wait pre-processing of \"\\([^\"]+\\)\"$"
  (lambda (filename)
    (let ((msg (format "lessopen: pre-processing %s done" filename))
          (-comare-fn #'string=))
      (cl-loop
       do (sit-for 1)
       until (-contains? (-map 's-trim ecukes-message-log) msg)))))
