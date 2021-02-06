function test1 (res)
    res2 := res;
    res3 := res2 + res2;
    return res3;
end function;

for x in [] do
    function test2 (res)
	res2 := res;
	res3 := res2 + res2;
	return res3;
    end function;
end for;

toto := function(test)
    a := 2+2;
    b := 2+2;
    return a+b;
end function;
;


procedure test3 (res)
    res2 := res;
    res3 := res2 + res2;
end procedure;

procedure test4 (res)
    res2 := res;
    res3 := res2 + res2;
end procedure;

intrinsic test5(res::Test) -> Test
{ }
    res2 := res;
    res3 := res;
    return res;
end intrinsic;


