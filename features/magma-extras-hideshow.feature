Feature: Magma support for hideshow
  In order to edit long magma code
  I want to have code folding

  Background: 
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode
    And I turn on hs-minor-mode

  Scenario: Fold an outer defun in and out (toggle)
    When I insert:
    """
    function toto (tata)
        2+2;
        if x eq 3 then
            3+3;
        else
            4+4;
        end if;
        function titi (tutu)
            return 5;
        end function;
        return 3;
    end function;
    6+6;
    """
    And I place the cursor before "function toto"
    And I toggle folding at point
    And I press "C-n M-m"
    Then the cursor should be before "6+6"
    When I press "C-p"
    And I toggle folding at point
    And I press "C-n M-m"
    Then the cursor should be before "2+2"
    
  Scenario: Fold an inner defun in and out (toggle)
    When I insert:
    """
    function toto (tata)
        2+2;
        if x eq 3 then
            3+3;
        else
            4+4;
        end if;
        function titi (tutu)
            return 5;
        end function;
        return 3;
    end function;
    6+6;
    """
    And I place the cursor before "function titi"
    And I toggle folding at point
    And I press "C-n M-m"
    Then the cursor should be before "return 3"
    When I press "C-p"
    And I toggle folding at point
    And I press "C-n M-m"
    Then the cursor should be before "return 5"
    
