Pascal compiler by Evan Schirle

What works:
    >Parses pascal code, converts everything to lowercase since pascal is not case-sensitive. 
    >global variables (only integers)
    >negative integers (must be surrounded by parenthesis!!)
    >If, while, for, block, and writeln statements all work. 
        *writeln only takes one argument, for loops can only ascend*
    >integer division with "/" works
    >remainder operator with "%" works
    >function definitions, function calls, and recursion works
        *if the programmer uses function names as variables outside of that function definition there will be bugs (error isnt handled b4 compilation)
    >function parameters are local variables to that function, throws an error if you try to use them outside of the function.

What doesn't work:
    >strings, boolean, char, or other variable types. (compiler assumes everything is an integer)

test.p 
    tests to make sure if, while, blocks, and printing works correctly. 


Things I could add:
    strings
    make writeln() accept multiple arguments. 
# Pascal_Compiler
