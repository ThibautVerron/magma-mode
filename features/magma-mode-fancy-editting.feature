Feature: Fancy editting features
  In order to edit magma code
  As a user
  I want to have fancy editting features

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
    And I place the cursor after "is "
    And I press "C-f"
    And I press "C-f"
    And I press "C-k"
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
    And I place the cursor after "is "
    And I press "C-f"
    And I press "C-f"
    And I press "C-f"
    And I press "C-f"
    And I press "C-k"
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
    Then I should see:
    """
      // This is a comment
      
    """

  Scenario: Fancy newline at the end of a C++ comment
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

