program fact1to20;

function fact(n : integer) : longint;
    begin
        if(n = 1) then fact :=n
        else fact := n * fact(n-1);
    end;

var
    n : integer = 20;
    i : integer;

begin
    for i := 1 to n do writeln(fact(i));
end.
