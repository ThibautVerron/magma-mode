# -*- indent-tabs-mode: nil; eval: (whitespace-mode 1) -*-

Feature: Magma code indentation
  In order to edit magma code
  As a user
  I want to have properly indented code
 
  Background:
    Given I am in buffer "*magma-test*"
    And the buffer is empty
    And I turn on magma-mode
    And I set the variable "indent-tabs-mode" to "nil"
    
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
    print 5;
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
            print 5;
        end case;
    end for;
    """

  Scenario: Indentation of if structures
    When I insert:
    """
    for x in list do
    if x eq 0 then
    print 1;
    print 2;
    elif x eq 1 then
    print 2;
    print 3;
    else
    print 3;
    print 4;
    end if;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in list do
        if x eq 0 then
            print 1;
            print 2;
        elif x eq 1 then
            print 2;
            print 3;
        else
            print 3;
            print 4;
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

  Scenario: Indentation in functions
    When I insert:
    """
    for x in l do
    function test (x,y : z := 5)
    x:=3+3;
    y:=4;
    return z;
    end function;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in l do
        function test (x,y : z := 5)
            x:=3+3;
            y:=4;
            return z;
        end function;
    end for;
    """

  Scenario: Indentation of function calls out of nowhere
    When I insert:
    """
    for x in l do
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
    for x in l do
        x := 2+2;
        Reverse(list);
        x := 2+2;
        y := Reverse(list);
        x := 2+2;
    end for;
    """

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

  @fixedbug
  Scenario: Indentation of expression suffixed with comments
    When I insert:
    """
    expr1;
    expr2;
    expr3; // comment1
    expr4;
    expr5; /* comment2 */
    expr6;
    """
    And I indent the buffer
    Then I should see:
    """
    expr1;
    expr2;
    expr3; // comment1
    expr4;
    expr5; /* comment2 */
    expr6;
    """

  @fixedbug
  Scenario: Indentation of expressions containing a "~"
    When I insert:
    """
    Reverse(~L);
    expr;
    """
    And I indent the buffer
    Then I should see:
    """
    Reverse(~L);
    expr;
    """
    
  Scenario: Indentation of select ... else
    When I insert:
    """
    for x in L do
    y := x eq 0 select 3 else 2;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        y := x eq 0 select 3 else 2;
    end for;
    """
    When I cut the line after "0"
    And I cut the line after "3"
    Then I should see:
    """
    for x in L do
        y := x eq 0
             select 3
             else 2;
    end for;
    """

  Scenario: Indentation of else in a combination of if and select
    When I insert:
    """
    for x in L do
    if x ge 0 then
    y := x eq 0 
    select 1
    else 2;
    else 
    y := x eq 0
    select 1
    else 2;
    end if;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        if x ge 0 then
            y := x eq 0 
                 select 1
                 else 2;
        else 
            y := x eq 0
                 select 1
                 else 2;
        end if;
    end for;
    """

  Scenario: Indentation of else in a combination of if, elif and select
    When I insert:
    """
    for x in L do
    if x ge 0 then
    y := x eq 0 
    select 1
    else 2;
    elif x ge 0 then
    y := x eq 0
    select 1
    else 2;
    else 
    y := x eq 0
    select 1
    else 2;
    end if;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        if x ge 0 then
            y := x eq 0 
                 select 1
                 else 2;
        elif x ge 0 then
            y := x eq 0
                 select 1
                 else 2;
        else 
            y := x eq 0
                 select 1
                 else 2;
        end if;
    end for;
    """
    
  Scenario: Indentation of else in a combination of case and select
    When I insert:
    """
    for x in L do
    case x:
    when 0:
    y := x eq 0
    select 1
    else 2;
    when 1:
    y := x eq 0
    select 1
    else 2;
    else
    y := x eq 0
    select 1
    else 2;
    end case;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        case x:
        when 0:
            y := x eq 0
                 select 1
                 else 2;
        when 1:
            y := x eq 0
                 select 1
                 else 2;
        else
            y := x eq 0
                 select 1
                 else 2;
        end case;
    end for;
    """

    
  @fixedbug
  Scenario: Indentation of type definitions in records
    When I insert:
    """
    x := recformat<toto : Type1, tata : Type2>;
    """
    And I cut the line after "Type1,"
    And I indent the buffer
    Then I should see:
    """
    x := recformat<toto : Type1,
                   tata : Type2>;
    """

  @fixedbug
  Scenario: Indentation of -> 
    When I insert:
    """
    testhom := hom<P -> Q>;
    """
    And I cut the line after "Q"
    And I indent the buffer
    Then I should see:
    """
    testhom := hom<P -> Q
                  >;
    """

  Scenario: Indentation of function definitions
    When I insert:
    """
    for x in l do
    function toto (a,
    b :
    c := 1,
    d := 2)
    x := a;
    return x;
    end function;
    toto := function (a,
    b :
    c := 1,
    d := 2)
    x := a;
    return x;
    end function;
    toto := function (a, b :
    c := 1,
    d := 2)
    x := a;
    return x;
    end function;
    toto := function (a, b :
    c := 1,
    d := 2)
    x := a;
    return x;
    end function;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in l do
        function toto (a,
                       b :
                       c := 1,
                       d := 2)
            x := a;
            return x;
        end function;
        toto := function (a,
                          b :
                          c := 1,
                          d := 2)
            x := a;
            return x;
        end function;
        toto := function (a, b :
                          c := 1,
                          d := 2)
            x := a;
            return x;
        end function;
        toto := function (a, b :
                          c := 1,
                          d := 2)
            x := a;
            return x;
        end function;
    end for;
    """

  Scenario: Indentation of procedure definitions
    When I insert:
    """
    for x in l do
    procedure toto (~a,
    b :
    c := 1,
    d := 2)
    x := a;
    y := b;
    end procedure;
    toto := procedure (~a,
    b :
    c := 1,
    d := 2)
    x := a;
    y := b;
    end procedure;
    procedure toto (~a, b :
    c := 1,
    d := 2)
    x := a;
    y := b;
    end procedure;
    toto := procedure (~a, b :
    c := 1,
    d := 2)
    x := a;
    y := b;
    end procedure;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in l do
        procedure toto (~a,
                        b :
                        c := 1,
                        d := 2)
            x := a;
            y := b;
        end procedure;
        toto := procedure (~a,
                           b :
                           c := 1,
                           d := 2)
            x := a;
            y := b;
        end procedure;
        procedure toto (~a, b :
                        c := 1,
                        d := 2)
            x := a;
            y := b;
        end procedure;
        toto := procedure (~a, b :
                           c := 1,
                           d := 2)
            x := a;
            y := b;
        end procedure;
    end for;
    """

  Scenario: Indentation of intrinsics
    When I insert:
    """
    intrinsic f(a::BoolElt, b::BoolElt
    : c := true, d := true) -> BoolElt
    { Docstring }
    a := 3;
    return a;
    end intrinsic;
    intrinsic f(a::BoolElt,
    b::BoolElt
    : c := true,
    d := true)
    -> BoolElt
    { Docstring }
    a := 3;
    return a;
    end intrinsic;
    intrinsic f(a::BoolElt,
    b::BoolElt
    : c := true,
    d := true)
    { Docstring }
    a := 3;
    return a;
    end intrinsic;
    """
    And I indent the buffer
    Then I should see:
    """
    intrinsic f(a::BoolElt, b::BoolElt
                : c := true, d := true) -> BoolElt
    { Docstring }
        a := 3;
        return a;
    end intrinsic;
    intrinsic f(a::BoolElt,
                b::BoolElt
                : c := true,
                  d := true)
              -> BoolElt
    { Docstring }
        a := 3;
        return a;
    end intrinsic;
    intrinsic f(a::BoolElt,
                b::BoolElt
                : c := true,
                  d := true)
    { Docstring }
        a := 3;
        return a;
    end intrinsic;
    """


  @fixedbug
  Scenario: Indentation in strings containing magma keywords
    When I insert:
    """
    eval Sprintf("x := 2+2;"
    cat "return 3;",
    5);
    """
    And I indent the buffer
    Then I should see:
    """
    eval Sprintf("x := 2+2;"
                 cat "return 3;",
                 5);
    """
   
  @fixedbug
  Scenario: Indentation in a set with multiple iterations + restriction
    When I insert:
    """
    x := {<a,b> : a in A, b in B 
    | test(a,b)}; 
    """
    And I indent the buffer
    Then I should see:
    """
    x := {<a,b> : a in A, b in B 
          | test(a,b)}; 
    """

  Scenario: Indentation of hanging assignments
    When I insert:
    """
    longvariablename := 
    2 + 2;
    """
    And I indent the buffer
    Then I should see:
    """
    longvariablename := 
        2 + 2;
    """

  @wishlist
  Scenario: Indentation of hanging parenthesed expressions
    When I insert:
    """
    x := [
    <a,b>  
    : a in A, b in B 
    | test 
    ];
    x := function (
    a,
    b 
    : c := 3
    ) 
    return a; 
    end function;
    """
    And I indent the buffer
    Then I should see:
    """
    x := [
        <a,b>  
        : a in A, b in B 
        | test 
    ];
    x := function (
            a,
            b 
            : c := 3
        ) 
        return a; 
    end function;
    """
    
  Scenario: Indentation in multi-assignments
    When I insert:
    """
    for x in L do
    longvar1,
    longvar2 := variable1
    + variable2;
    end for;
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        longvar1,
        longvar2 := variable1
                    + variable2;
    end for;
    """

  # Fixed in commit d7eef14a7cd64a1f67333dc4386f377a98d45574
  @bugfix
  Scenario: Indentation of complex expressions
    When I insert:
    """
    for x in L do
    printf "x = %%%%o, "
    cat "y = %%%%o\n",
    x, y;
    
    printf
    "x = %%%%o, "
    cat "y = %%%%o\n",
    x, y;
    
    vprintf
    User1:
    "x = %%%%o,"
    cat "y = %%%%o\n",
    x, y;

    vprintf User1:
    "x = %%%%o,"
    cat "y = %%%%o\n",
    x, y;
    
    vprintf User1: "x = %%%%o,"
    cat "y = %%%%o\n",
    x, y;
    end for;
    
    """
    And I indent the buffer
    Then I should see:
    """
    for x in L do
        printf "x = %%%%o, "
               cat "y = %%%%o\n",
               x, y;
    
        printf
            "x = %%%%o, "
            cat "y = %%%%o\n",
            x, y;
    
        vprintf
            User1:
            "x = %%%%o,"
            cat "y = %%%%o\n",
            x, y;
    
        vprintf User1:
            "x = %%%%o,"
            cat "y = %%%%o\n",
            x, y;
    
        vprintf User1: "x = %%%%o,"
                       cat "y = %%%%o\n",
                       x, y;
    end for;
    """

  #Fixed with commit 79cd53390807b567482e4d97d15fb5c178b7aa5c
  @bugfix
  Scenario: SMIE movement over sexps
    When I insert:
    """
    2+2;R<x,y> := Polynomial_Ring(foo,bar); 3+3;
    """
    And I place the cursor before "olynomial"
    When I press "M-{"
    Then the cursor should be before "R<x"
    When I press "M-}"
    Then the cursor should be after "bar);"

  #Fixed with commit 733446922740253da63191689db30d6409c87474
  @bugfix
  Scenario: Indentation in repeat... until
    When I insert:
    """
    repeat
    x := x+1;
    y := y+1;
    until x eq 5;
    """
    And I indent the buffer
    Then I should see:
    """
    repeat
        x := x+1;
        y := y+1;
    until x eq 5;
    """

  #Tentative fix in commit ecc38038a86d04bf3d1bcf8aee3ab82f5168e535
  @bugfix
  Scenario: SMIE movement from inside sexps
    When I insert:
    """
    2+2;L := [x,y,z]; 3+3;
    """
    And I place the cursor before "y"
    When I press "M-{"
    Then the cursor should be before "L"
    When I press "M-}"
    Then the cursor should be after "z];"

  @bugfix
  Scenario: indentation of function body with subword-mode
    When I insert:
    """
    function Test_TestTest (a,b,c)
    return 2;
    end function;
    """
    And I turn on subword-mode
    And I place the cursor before "return"
    And I press "C-i"
    Then I should see:
    """
    function Test_TestTest (a,b,c)
        return 2;
    end function;
    """

  @bugfix
  Scenario: indentation after while
    When I insert:
    """
    while x eq 1 do
    toto;
    end while;
    """
    And I place the cursor before "toto"
    And I indent the buffer
    Then I should see:
    """
    while x eq 1 do
        toto;
    end while;
    """

  @bugfix
  Scenario: indentation in record fields after ':'
    When I insert:
    """
    XX := recformat<x : type1,
    y : type2>;
    """
    And I place the cursor before "y"
    And I indent the buffer
    Then I should see:
    """
    XX := recformat<x : type1,
                    y : type2>;
    """

  @bugfix
  Scenario: indentation after keywords in symbols
    When I insert:
    """
    function test_random(a,b)
    return 3;
    end function;
    """
    And I place the cursor before "return"
    And I indent the buffer
    Then I should see:
    """
    function test_random(a,b)
        return 3;
    end function;
    """

  @bugfix
  Scenario: indentation after special names in symbols
    When I insert:
    """
    intrinsic Print(obj::Any)
    { }
    ;
    end intrinsic;
    """
    And I place the cursor before "{ }"
    And I indent the buffer
    And I place the cursor before ";"
    And I indent the buffer
    Then I should see:
    """
    intrinsic Print(obj::Any)
    { }
        ;
    end intrinsic;
    """

  @wishlist
  Scenario: indentation with intrinsics with composite type
    When I insert:
    """
    intrinsic test(a::SeqEnum[RngIntElt]) -> SeqEnum[BoolElt], BoolElt
    {}
    return [],3;
    end intrinsic;

    intrinsic test(a::SeqEnum[RngIntElt]) 
    -> SeqEnum[BoolElt], BoolElt
    {}
    return [],3;
    end intrinsic;

    intrinsic test(a::SeqEnum[RngIntElt]) 
    -> SeqEnum[BoolElt], 
    BoolElt
    {}
    return [],3;
    end intrinsic;
    """
    And I indent the buffer
    Then I should see:
    """
    intrinsic test(a::SeqEnum[RngIntElt]) -> SeqEnum[BoolElt], BoolElt
        {}
        return [],3;
    end intrinsic;

    intrinsic test(a::SeqEnum[RngIntElt]) 
                  -> SeqEnum[BoolElt], BoolElt
        {}
        return [],3;
    end intrinsic;

    intrinsic test(a::SeqEnum[RngIntElt]) 
                  -> SeqEnum[BoolElt], 
                     BoolElt
        {}
        return [],3;
    end intrinsic;
    """
