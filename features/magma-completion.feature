Feature: Magma completion mechanism
  In order to use magma with comint in emacs
  I will need to have a proper completion engine

  Background: 
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode 
    When I press "C-c C-o"
    And I press "C-x o"
    And I wait for 1 second
    
  Scenario: Completion at the end of a line, one candidate
    Given I insert "SetVerbo"
    And I press "TAB"
    Then I should see "> SetVerbose"
    When I press "RET"
    And I wait for 1 second
    Then I should see "Input: SetVerbose"
    And I should not see "Input: SetVerboSetVerbose"
    

  Scenario: Completion at the end of a line, multiple candidates
    Given I press "RET"
    And I insert "Set"
    And I press "TAB"
    Then I should see message "Complete, but not unique"
    When I type "A"
    And I press "TAB"
    And I switch to buffer "*Completions*"
    Then I should see "SetAllInvariantsOfDegree"
    And I should see "SetAssertions"
    And I should not see "SetBufferSize"
    When I place the cursor before "AllInvariants"
    And I press "RET"
    Then I should be in buffer "*magma*"
    And I should see "SetAllInvariantsOfDegree"
    
  Scenario: Completion in the middle of a line, one candidate
    Given I press "RET"
    And I insert "toto(SetVerbo)tata"
    And I place the cursor before ")tata"
    And I press "TAB"
    Then I should see "> toto(SetVerbose)tata"
    When I press "RET"
    And I wait for 1 second
    Then I should see "Input: toto(SetVerbose)tata"
    And I should not see "Input: toto(SetVerbo)tatatoto(SetVerbose)tata"
    And I should not see "Input: toto(SetVerboSetVerbose)tata"
    
  Scenario: Completion in the middle of a line, multiple candidates
    
  Scenario: Picking candidates from the manual index
    Given I press "RET"
    And I insert "SetVerbo"
    And I press "TAB"
    Then I should see "SetVerbose"
    
  Scenario: Picking candidates from the user input


  
