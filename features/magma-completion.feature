Feature: Magma completion mechanism
  In order to use magma with comint in emacs
  I will need to have a proper completion engine

  Background: 
    Given I start a magma process
    And I wait for an instant
    And I press "RET"
    And I wait for an instant

  Scenario: Completion at the end of a line, one candidate
    Given I insert "SetVerbo"
    And I press "TAB"
    Then I should see "> SetVerbose"
    When I press "RET"
    And I wait for an instant
    Then I should see "Input: SetVerbose"
    And I should not see "Input: SetVerboSetVerbose"
    
  Scenario: Completion at the end of a line, multiple candidates
    Given I insert "Set"
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
    Given I insert "toto(SetVerbo)tata"
    And I place the cursor before ")tata"
    And I press "TAB"
    Then I should see "> toto(SetVerbose)tata"
    When I press "RET"
    And I wait for an instant
    Then I should see "Input: toto(SetVerbose)tata"
    And I should not see "Input: toto(SetVerbo)tatatoto(SetVerbose)tata"
    And I should not see "Input: toto(SetVerboSetVerbose)tata"
    
  Scenario: Completion in the middle of a line, multiple candidates
    
  Scenario: Picking candidates from the manual index
    Given I insert "SetVerbo"
    And I press "TAB"
    Then I should see "SetVerbose"
    
  Scenario: Picking candidates from the user input
    Given I insert:
    """
    function myfunction (res) return res; end function;
    """
    And I press "RET"
    And I wait for an instant
    And I insert "myf"
    And I press "TAB"
    Then I should see "> myfunction"
  
