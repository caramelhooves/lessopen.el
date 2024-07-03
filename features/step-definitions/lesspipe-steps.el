(Then "^I open \"\\([^\"]+\\)\" with lessopen$"
  (lambda (filename)
    (view-file-with-lessopen filename)))

(Then "^I wait pre-processing of \"\\([^\"]+\\)\"$"
  (lambda (filename)
    (let ((msg (format "lessopen: pre-processing %s done" filename))
          (-comare-fn #'string=))
      (cl-loop
       repeat 3
       do (sit-for 1)
       until (-contains? (-map 's-trim ecukes-message-log) msg))
      (cl-assert (-contains? (-map 's-trim ecukes-message-log) msg) nil "Expected %s to be included in %s"
                 msg (-map 's-trim ecukes-message-log)))))
