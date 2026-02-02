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
    std::cout << "Hello\nWorld!" << std::endl;
}

int main() {
    myFunction();
    std::cout << "Testing 123" << std::endl;
    return 0;
}
