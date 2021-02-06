function test1 (res)
    res2 := res;
    res3 := res2 + res2;
    return res3;
end function;

function test2 (res)
    res2 := res;
res3 := res2 + res2;
return res3;
end function;

function test (x,y : z := 5)
    x:=3+3;
y := 4;
    return z;
end function;
for x in l do
    function test (x,y : z := 5)
	x:=3+3;
    y := 4;
    return z;
    end function;
end for;


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
