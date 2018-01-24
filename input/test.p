program testEverything;

function sum(a : integer, b : integer) : integer;
    begin
        sum := a + b;
    end;

var
    x : integer = 5;

begin
    if (1>0) then
        writeln(20000)
    else
        writeln(0);

    while(x>0) do
        begin
            writeln(x);
            x := x - 1;
        end;

    writeln(1111111111);

    for x:=0 to 20 do
        writeln(x);
    
    writeln((-3333333));

    writeln(sum(3, sum(2, 1)));

    writeln(sum(x, 100));

    writeln(3+(-3));
end.
