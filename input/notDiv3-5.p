program notDiv3or5;

var 
    i : integer;
    dummy : integer;

begin
    for i:=1 to 100 do
        if ((3%i)>0) then 
            if((5%i)>0) then 
                writeln(i) 
            else dummy:=0 
        else dummy:=i;
end.
