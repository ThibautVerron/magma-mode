Feature: Magma code indentation
  In order to edit magma code
  As a user
  I want to have properly indented code
  
  Background:
    Given I am in buffer "*magma-test*"
    And I turn on magma-mode
    And the buffer is empty
    
  Scenario: Basic indentation
    When I insert:
    """
    2+2;
    3+3;
    """
    And I indent line "1"
    Then I should not see:
    """
        2+2;
    3+3;
    """
    When I indent line "2"
    Then I should not see:
    """
    2+2;
        3+3;
    """

  Scenario: Indentation of expressions in blocks
    When I insert:
    """
    for x in list do
    a;
    end for;
    """
    And I indent line "2"
    Then I should see:
    """
    for x in list do
        a;
    end for;
    """
  
  Scenario: Indentation of empty lines in blocks
    When I insert:
    """
    for x in list do
    
    end for;
    """
    And I indent line "2"
    And I press "a"
    Then I should see:
    """
    for x in list do
        a
    end for;
    """
    
    @wishlist
  Scenario: Indentation of C comments in blocks
    When I insert:
    """
    for x in list do
    /* Comment */
    end for;
    """
    And I indent line "2"
    Then I should see:
    """
    for x in list do
        /* Comment */
    end for;
    """
  
  Scenario: Indentation of C++ comments in blocks
    When I insert:     
    """
    for x in list do
    // Comment
    end for;
    """
    And I indent line "2"
    Then I should see:
    """
    for x in list do
        // Comment
    end for;
    """
    
  Scenario: Basic indentation of S-expressions
    When I insert:
    """
    x := [toto,tata];
    """
    And I cut the line before "tata"
    Then I should see:
    """
    x := [toto,
          tata];
    """

  Scenario: Indentation of S-expressions with arbitrary spacing
    When I insert:
    """
    x := [  tutu, tete]
    """
    And I cut the line before "tete"
    Then I should see:
    """
    x := [  tutu, 
            tete]
    """
    
  @wishlist
  Scenario: Indentation of assignments
    When I insert:
    """
    longvariablename := 2 + 2;
    """
    And I cut the line before "+"
    Then I should see:
    """
    longvariablename := 2
        + 2;
    """
    
  Scenario: Indentation of expressions without parens
    When I insert:
    """
    print "toto" cat "tata";
    """
    And I cut the line before "cat"
    Then I should see:
    """
    print "toto" 
          cat "tata";
    """
    
  @wishlist
  Scenario: Indentation of expressions with colon
    When I insert:
    """
    vprintf Test : "toto %%%%o", tata; 
    """
    And I cut the line before "tata"
    Then I should see:
    """
    vprintf Test : "toto %%%%o", 
            tata; 
    """

  Scenario: Indentation of case switch
    When I insert:
    """
    for x in list do
    case x :
    when 1 :
    print 1;
    when 2 :
    print 2;
    print 3;
    else
    print 4;
    end case;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in list do
        case x :
        when 1 :
            print 1;
        when 2 :
            print 2;
            print 3;
        else
            print 4;
        end case;
    end for;
    """

  Scenario: Indentation of if structures
    When I insert:
    """
    for x in list do
    if x eq 0 then
    print 1;
    elif x eq 1 then
    print 2;
    else
    print 3;
    end if;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in list do
        if x eq 0 then
            print 1;
        elif x eq 1 then
            print 2;
        else
            print 3;
        end if;
    end for;
    """

  Scenario: Indentation of try structures
    When I insert:
    """
    for x in list do
    try 
    print 2+2;
    print 1/0;
    catch e
    3+3;
    print 2;
    end try;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in list do
        try 
            print 2+2;
            print 1/0;
        catch e
            3+3;
            print 2;
        end try;
    end for;
    """
    
  @wishlist
  Scenario: Indentation of colons in s-exps
    When I insert:
    """
    x := test(y : z := 3, longvariablename := 5 + t);
    """
    And I cut the line before "z"
    And I cut the line before "long"
    And I cut the line before "+"
    Then I should see:
    """
    x := test(y : 
              z := 3, 
              longvariablename := 5 
                  + t);
    """
    
  Scenario: Indentation of pipes in s-exps
    When I insert:
    """
    k := [x : x in l | false or x eq 0]
    """
    And I cut the line before "false"
    And I cut the line before "or x"
    And I cut the line before "]"
    Then I should see:
    """
    k := [x : x in l | 
          false 
          or x eq 0
         ]
    """
  
  @wishlist
  Scenario: Indentation before operators
    When I insert:
    """
    x := 1 + 2 - 3;
    """
    And I cut the line before "+"
    Then I should see:
    """
    x := 1
         + 2 - 3;
    """
    When I clear the buffer
    And I insert:
    """
    x := 1 + 2 * 3;
    """
    And I cut the line before "*"
    Then I should see:
    """
    x := 1 + 2 
             * 3;
    """

  @wishlist
  Scenario: Indentation after operators
    When I insert:
    """
    x := 1 + 2 - 3;
    """
    And I cut the line before "2"
    Then I should see:
    """
    x := 1 +
            2 - 3;
    """

  @function
  Scenario: Indentation in functions
    When I insert:
    """
    function test (x,y : z := 5)
    x:=3+3;
    return z;
    end function;
    """
    And I indent the buffer
    Then I should see:
    """
    function test (x,y : z := 5)
        x:=3+3;
        return z;
    end function;
    """

  @function
  Scenario: Indentation of function calls out of nowhere
    When I insert:
    """
    x := 2+2;
    Reverse(list);
    x := 2+2;
    y := Reverse(list);
    x := 2+2;
    """
    And I indent the buffer
    Then I should see:
    """
    x := 2+2;
    Reverse(list);
    x := 2+2;
    y := Reverse(list);
    x := 2+2;
    """

  @function
  Scenario: Indentation of function calls in a block
    When I insert:
    """
    for x in list do
    x := 2+2;
    Reverse(list);
    x := 2+2;
    y := Reverse(list);
    x := 2+2;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in list do
        x := 2+2;
        Reverse(list);
        x := 2+2;
        y := Reverse(list);
        x := 2+2;
    end for;
    """
    
  @function
  Scenario: Indentation of function calls in a function block
    When I insert:
    """
    function test(toto)
    x := 2+2;
    Reverse(list);
    x := 2+2;
    y := Reverse(list);
    x := 2+2;
    end function;
    """
    And I indent the buffer
    Then I should see:
    """
    function test(toto)
        x := 2+2;
        Reverse(list);
        x := 2+2;
        y := Reverse(list);
        x := 2+2;
    end function;
    """

