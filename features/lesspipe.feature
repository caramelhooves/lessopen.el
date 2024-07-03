Feature: Use lesspipe
  Scenario: open compressed file
  Given I switch to buffer "*scratch*"
  Then I open "testdata/test.txt.gz" with lessopen
  Then I should be in buffer "test.txt.gz"
  And the cursor should be at point "1"
  And I wait pre-processing of "test.txt.gz"
  Then I should see "This"

Scenario: open plaintext file
  Given I switch to buffer "*scratch*"
  Then I open "testdata/test.txt" with lessopen
  Then I should be in buffer "test.txt"
  And the cursor should be at point "1"
  And I wait pre-processing of "test.txt"
  Then I should see "Plain text files should not be changed by lesspipe."
