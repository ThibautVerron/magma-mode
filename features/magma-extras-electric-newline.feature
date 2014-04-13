Feature: Fancy editting features
  In order to edit magma code
  As a user
  I want that newline has a special behavior

  Background:
    Given I am in buffer "*magma-test*"
    And I turn on magma-mode
    And the buffer is empty
  
  Scenario: Newlines in string code
    When I insert:
    """
    x := "this is a string";
    """
    And I go to beginning of buffer
    And I place the cursor between "is " and "a"
    And I press "RET"
    And I press "C-a"
    And I press "C-b"
    And I press "C-d"
    And I press "M-<SPC>"
    Then I should see:
    """
    x := "this is " cat "a string";
    """

  Scenario: Displayed newlines in string code
    When I insert:
    """
    x := "this is a string";
    """
    And I go to beginning of buffer
    And I place the cursor between "is " and "a"
    And I press "C-c C-j"
    And I press "C-a"
    And I press "C-b"
    And I press "C-d"
    And I press "M-<SPC>"
    Then I should see:
    """
    x := "this is \n" cat "a string";
    """
  
  Scenario: Newline in the middle of a C++ comment
    When I insert:
    """
    // This is a comment
    """
    And I go to beginning of buffer
    And I place the cursor between "is " and "a"
    And I press "RET"
    Then I should see:
    """
    // This is 
    // a comment
    """

  Scenario: Newline at the end of a C++ comment
    When I insert:
    """
    // This is a comment
    """
    And I go to beginning of buffer
    And I place the cursor after "comment"
    And I press "RET"
    And I press "a"
    Then I should see:
    """
    // This is a comment
    a
    """

  Scenario: Modified newline at the end of a C++ comment
    When I insert:
    """
    // This is a comment
    """
    And I go to beginning of buffer
    And I place the cursor after "comment"
    And I press "C-c C-j"
    Then I should see:
    """
    // This is a comment
    // 
    """

