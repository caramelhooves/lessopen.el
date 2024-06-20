Feature: Use lesspipe
  Scenario: open compressed file
  Given I switch to buffer "*scratch*"
  Then I open "testdata/test.txt.gz" with lessopen
  Then I should be in buffer "testdata/test.txt.gz|less"
  And the cursor should be at point "1"
  Then I should see "This"
