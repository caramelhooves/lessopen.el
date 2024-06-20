(Then "^I open \"\\([^\"]+\\)\" with lessopen$"
  (lambda (filename)
    (find-file-with-lessopen-sync filename)
    ))
