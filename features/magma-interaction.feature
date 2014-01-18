Feature: Interaction with a magma process
  In order to edit magma code
  As a user
  I want to be able to evaluate code from within emacs

  Background: 
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode 
    When I press "C-c C-o"
    And I wait for 1 second
   
  Scenario: Start a magma process
    When I press "C-x o"
    Then I should be in buffer "*magma*"
    And I should see "Welcome to dummymagma v1.0!"
    And I should see "> "

  Scenario: Evaluate an external buffer
    Given I insert "6*7;"
    And I press "C-c C-b"
    And I press "C-x o"
    And I wait for 1 second
    Then I should see "> 6*7;"
    And I should not see:
    """
    > 6*7;
    6*7;
    """
    And I should see "42"

  Scenario: Evaluate an expression in the magma buffer
    Given I press "C-x o"
    And I press "M->"
    And I insert "6*7;"
    And I press "RET"
    And I wait for 1 second
    Then I should see "> 6*7;"
    And I should not see:
    """
    > 6*7;
    6*7;
    """
    And I should see "42"
