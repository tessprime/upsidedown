// Test file for xprefix_renamer
#include <iostream>

int myVariable = 42;

struct MyStruct {
    int myField;
};

void myFunction() {
    int localVar = myVariable;
    MyStruct s;
    s.myField = localVar;
}

int main() {
    myFunction();
    return 0;
}
