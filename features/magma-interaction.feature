Feature: Interaction with a magma process
  In order to edit magma code
  As a user
  I want to be able to evaluate code from within emacs

  Background: 
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode 
    When I press "C-c C-o"
    And I wait for an instant
   
  Scenario: Start a magma process
    When I press "C-x o"
    Then I should be in buffer "*magma*"
    And I should see "Welcome to dummymagma v1.0!"
    And I should see "> "

  Scenario: Evaluate an external buffer, no reecho
    Given I insert "6*7;"
    And I press "C-c C-b"
    And I press "C-x o"
    And I wait for an instant
    Then I should see "> 6*7;"
    And I should not see:
    """
    > 6*7;
    6*7;
    """
    And I should see "Input: 6*7;"

  Scenario: Evaluate an expression in the magma buffer, no reecho
    Given I press "C-x o"
    And I press "M->"
    And I insert "6*7;"
    And I press "RET"
    And I wait for an instant
    Then I should see "> 6*7;"
    And I should not see:
    """
    > 6*7;
    6*7;
    """
    And I should see "Input: 6*7;"

  Scenario: Evaluate a simple expression
    Given I insert:
    """
    x := 3;
    y := 4;
    """
    And I place the cursor before "x"
    And I press "C-c C-e"
    And I press "C-x o"
    And I wait for an instant
    Then I should see:
    """
    x := 3;
    """
    But I should not see:
    """
    y := 4;
    """

  Scenario: Evaluate a complex expression (for... end for)
    Given I insert:
    """
    for i in list do
    x := 3;
    end for;
    y := 4;
    """
    And I place the cursor before "for"
    And I press "C-c C-e"
    And I press "C-x o"
    And I wait for an instant
    Then I should see:
    """
    for i in list do
    """
    And I should see:
    """
    x := 3;
    """
    And I should see:
    """
    end for;
    """
    But I should not see:
    """
    y := 4;
    """

  Scenario: Evaluate a complex expression (if ... (select... else) else... end if)
    Given I insert:
    """
    if test then
    x := test2 select 1 else 2;
    else
    y := test2 select 1 else 2;
    end if;
    z := 4;
    """
    And I place the cursor before "x"
    And I press "C-c C-e"
    And I press "C-x o"
    And I wait for an instant
    Then I should see:
    """
    x := test2 select 1 else 2;
    """
    But I should not see:
    """
    y := test2 select 1 else 2;
    """
    When I switch to buffer "*magma-test*"
    And I place the cursor before "if"
    And I press "C-c C-e"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see:
    """
    if test then
    """
    And I should see:
    """
    y := test2 select 1 else 2;
    """
    And I should see:
    """
    end if;
    """
    But I should not see:
    """
    z := 4;
    """

  Scenario: Evaluate a defun
    Given I insert:
    """
    function toto (arg)
    arg := arg +2;
    return arg;
    end function;
    y := 4;
    """
    And I place the cursor before "+2"
    And I press "C-c C-f"
    And I press "C-x o"
    And I wait for an instant
    Then I should see:
    """
    function toto (arg)
    """
    And I should see:
    """
    arg := arg +2;
    """
    And I should see:
    """
    return arg;
    """
    And I should see:
    """
    end function;
    """
    But I should not see:
    """
    y := 4;
    """
    ;; And I should not see message "Not in a function, procedure or intrinsics definition"

  Scenario: Evaluate a defun, not in a defun
    Given I insert:
    """
    x := 3;
    """
    And I place the cursor before "x"
    And I press "C-c C-f"
    And I press "C-x o"
    And I wait for an instant
    Then I should see message "Not in a function, procedure or intrinsics definition"

    
  Scenario: Start multiple processes


  Scenario: Send expressions to multiple processes, separately


  Scenario: Send a line to multiple processes, broadcast


  Scenario: Send an expression to multiple processes, broadcast


  Scenario: Send a region to multiple processes, broadcast

