Feature: Interaction with a magma process
  In order to edit magma code
  As a user
  I want to be able to evaluate code from within emacs

  Background: 
    Given I am in buffer "*magma*"
    And the buffer is empty
    And I switch to buffer "*magma-2*"
    And the buffer is empty
    And I switch to buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode 
    And I press "C-c C-a"
    And I wait for an instant
    And I switch to buffer "*magma*"
    And I wait for an instant
    And I press "RET"
    And I wait for an instant
   
  Scenario: Start a magma process
    Then I should be in buffer "*magma*"
    And I should see "Welcome to dummymagma v1.0!"
    And I should see "> "
    And the buffer should have a process

    
  Scenario: Start multiple processes
    Given I am in buffer "*magma-test*"
    And I press "C-u 2 C-c C-o"
    Then I should be in buffer "*magma-2*"
    When I press "RET"
    And I wait for an instant
    Then I should see "Welcome to dummymagma v1.0!"
    And I should see "> "
    And the buffer should have a process
    

  Scenario: Evaluate an external buffer, no reecho
    Given I am in buffer "*magma-test*"
    And I insert "6*7;"
    And I press "C-c C-b"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should not see:
    """
    > 6*7;
    > 6*7;
    """
    And I should see "Input: 6*7;"

  Scenario: Evaluate an expression in the magma buffer, no reecho
    Given I press "M->"
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
    Given I am in buffer "*magma-test*"
    And I insert:
    """
    x := 3;
    y := 4;
    """
    And I place the cursor before "x"
    And I press "C-c C-e"
    And I switch to buffer "*magma*"
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
    Given I am in buffer "*magma-test*"
    And I insert:
    """
    for i in list do
    x := 3;
    end for;
    y := 4;
    """
    And I place the cursor before "for"
    And I press "C-c C-e"
    And I switch to buffer "*magma*"
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
    Given I am in buffer "*magma-test*"
    And I insert:
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
    And I switch to buffer "*magma*"
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
    Given I am in buffer "*magma-test*"
    And I insert:
    """
    function toto (arg)
    arg := arg +2;
    return arg;
    end function;
    y := 4;
    """
    And I place the cursor before "+2"
    And I press "C-c C-f"
    Then the cursor should be after "function;"
    When I switch to buffer "*magma*"
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
    ;; And I should not see message "Not in a function, procedure or intrinsic definition"

  Scenario: Evaluate a defun, not in a defun
    Given I am in buffer "*magma-test*"
    And I insert:
    """
    function toto (arg)
    arg := arg +2;
    return arg;
    end function;
    x := 3;
    """
    And I place the cursor before "x"
    And I press "C-c C-f"
    And I wait for an instant
    Then I should see message "Not in a function, procedure or intrinsic definition"
    And the cursor should be before "x"

  Scenario: Evaluate an expression with no output
    Given I am in buffer "*magma-test*"
    And I insert:
    """
    silence;
    """
    And I place the cursor before "s"
    And I press "C-c C-e"
    And I switch to buffer "*magma*"
    And I insert "@RandomText1@"
    And I wait for an instant
    Then I should see:
    """
    > @RandomText1@
    """
    But I should not see:
    """
    Input: silence;
    """
    

    
  Scenario: Send expressions to multiple processes, separately
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I insert:
    """
    2+2;
    3+3;
    """
    And I place the cursor before "2+"
    And I press "C-c C-e"
    And I place the cursor before "3+"
    And I press "C-u 2 C-c C-e"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see "Input: 2+2;"
    And I should not see "Input: 3+3;"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then I should see "Input: 3+3;"
    And I should not see "Input: 2+2;"
    

  Scenario: Send a line to multiple processes, broadcast
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I insert:
    """
    4+4;
    """
    And I place the cursor before "4+"
    And I press "C-u C-u C-c C-l"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see "Input: 4+4;"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then I should see "Input: 4+4;"


  Scenario: Send an expression to multiple processes, broadcast
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I insert:
    """
    5+5;
    """
    And I place the cursor before "5+"
    And I press "C-u C-u C-c C-e"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see "Input: 5+5;"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then I should see "Input: 5+5;"
    

  Scenario: Send a region to multiple processes, broadcast
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I insert:
    """
    6; 
    """
    And I select "6;"
    And I press "C-u C-u C-c C-e"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see "Input: 6;"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then I should see "Input: 6;"
    


  Scenario: Output filter: debugger prompt
    Given I am in buffer "*magma*"
    And I press "RET"
    And I wait for an instant
    And I insert "debug;"
    And I press "RET"
    And I wait for an instant
    And I wait for an instant T
    Then I should see "debug> "
    And I should not see:
    """
    > debug;
    > debug;
    """
    Then I insert "quit;"
    And I press "RET"

    
  Scenario: Evaluation of a region as a whole
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I set magma-interactive-method to whole
    And I set magma-interactive-wait-between-inputs to t
    # And I show the result of "magma-interactive-method"
    # And I show the result of "magma-interactive-wait-between-inputs"
    # And I show the result of "magma-working-buffer-number"
    # And I show the result of "(magma-make-buffer-name)"
    # And I show the result of "magma-interactive-use-load"
    And I insert:
    """
    whole_test: 1+1;
    whole_test: 2+2;
    
    """
    Then I should see:
    """
    whole_test: 1+1;
    whole_test: 2+2;
    
    """
    When I press "C-c C-b"
    # And I wait for an instant
    And I switch to buffer "*magma*"
    # And I press "RET"
    And I wait for an instant
    Then I should see:
    """
    > whole_test: 1+1;
    whole_test: 2+2;
    """
    And I should see:
    """
    Input: whole_test: 1+1;
    Input: whole_test: 2+2;
    """
    
  Scenario: Evaluation of a region line by line
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I set magma-interactive-method to line
    And I set magma-interactive-wait-between-inputs to t
    And I insert:
    """
    line_test: 1+1;
    line_test: 2+2;
    """
    And I press "C-c C-b"
    # And I wait for an instant
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see:
    """
    > line_test: 1+1;
    Input: line_test: 1+1;
    > line_test: 2+2;
    Input: line_test: 2+2;
    """
    
  Scenario: Evaluation of a region expression by expression
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I set magma-interactive-method to expr
    And I set magma-interactive-wait-between-inputs to t
    And I insert:
    """
    for expr_test in foo do
        bar;
    end for;
    expr_test: 2+2;
    """
    And I press "C-c C-b"
    # And I wait for an instant
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see:
    """
    > for expr_test in foo do
        bar;
    end for;
    Input: for expr_test in foo do
    Input:     bar;
    Input: end for;
    > expr_test: 2+2;
    Input: expr_test: 2+2;
    """

    
  Scenario: Kill a magma process
    Given I am in buffer "*magma-test*"
    And I press "C-c C-k"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma killed"


  Scenario: Interrupt a magma process
    Given I am in buffer "*magma-test*"
    And I press "C-c C-i"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma interrupt"
    

  Scenario: Kill multiple magma processes
    Given I am in buffer "*magma-test*"
    And I press "C-c C-a"
    And I press "C-u 2 C-a"
    And I press "C-u C-u C-c C-k"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma killed"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma-2 killed"
    
    
  Scenario: Interrupt multiple magma processes
    Given I am in buffer "*magma-test*"
    And I press "C-c C-a"
    And I press "C-u 2 C-c C-a"
    And I switch to buffer "*magma-2*"
    Then the buffer should have a process
    When I switch to buffer "*magma-test*"
    And I wait for an instant
    And I press "C-u C-u C-c C-i"
    And I wait for an instant
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma interrupt"
    When I switch to buffer "*magma-2*"
    And I wait for an instant
    Then the buffer should have no process
    And I should see "Process magma-2 interrupt"

  Scenario: Errors in magma evaluation
    Given I am in buffer "*magma*"
    When I insert "error-absolute;"
    And I wait for an instant
    And I press "RET"
    And I wait for an instant
    Then I should see pattern:
    """
    In file ".+/error\.m", line [0-9]+, column [0-9]+:
    Error
    """
    And I place the cursor before "error.m"
    And I wait for an instant
    Then current point should have the compilation-error face
    When I press "C-x `"
    Then I should be in buffer "error.m"
    And the cursor should be after "abc"
    And the cursor should be before "def"
    When I switch to buffer "*magma*"
    And I go to end of buffer
    And I insert "error-relative;"
    And I wait for an instant
    And I press "RET"
    And I wait for an instant
    Then I should see pattern:
    """
    In file ".+/error\.m", line [0-9]+, column [0-9]+:
    Error
    """
    And I place the cursor before "error.m"
    And I wait for an instant
    Then current point should have the compilation-error face
    When I press "C-x `"
    Then I should be in buffer "error.m"
    And the cursor should be after "123"
    And the cursor should be before "456"
    
  # Tentative fix in commit 91b1280cc8709a8b1e699ff8d7e3a0272b7198aa
  # Does not fail when run manually
  # Does not fail with --only-failing
  @bugfix
  @unreproducible
  Scenario: Behavior at end of buffer
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I set magma-interactive-method to line
    And I insert "a:=2+2;" 
    And I place the cursor before "a"
    When I go to end of buffer
    Then the cursor should be after ";"
    ### Heisenbug here...
    When I place the cursor before "a"
    And I press "C-c C-e"
    And I wait for an instant
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see "Input: a:=2+2;"

  @bugfix
  Scenario: Printing a fake prompt
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I insert:
    """
    print "> ";
    print 3;
    """
    And I press "C-c C-b"
    And I switch to buffer "*magma*"
    And I wait for an instant
    Then I should see:
    """
    > print "> ";
    > 
    > print 3;
    3
    """
    
