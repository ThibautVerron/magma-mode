@wishlist
Feature: Magma completion mechanism
  In order to use magma with comint in emacs
  I will need to have a proper completion engine

  Background: 
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode 
    When I press "C-c C-o"
    And I wait for 1 second
    And I press "C-x C-o"


  Scenario: Completion at the end of a line, one candidate
    Given I insert "SetVerbo"
    And I start an action chain
    And I press "TAB"
    And I press "RET"
    And I execute the action chain
    Then I should see "> SetVerbose"
    When I press "RET"
    Then I should see "SetVerbose"
    And I should not see "SetVerboSetVerbose"

  Scenario: Completion at the end of a line, multiple candidates
    

  Scenario: Completion in the middle of a line, one candidate
    Given I insert "toto(SetVerbo)tata"
    And I place the cursor before ")tata"
    And I start an action chain
    And I press "TAB"
    And I press "RET"
    And I execute the action chain
    Then I should see "> toto(SetVerbose)tata"
    When I press "RET"
    Then I should see "toto(SetVerbose)tata"
    And I should not see "toto(SetVerbo)tatatoto(SetVerbose)tata"
    And I should not see "toto(SetVerboSetVerbose)tata"
    

  Scenario: Completion in the middle of a line, multiple candidates


  Scenario: Picking candidates from the manual index


  Scenario: Picking candidates from the user input


  
