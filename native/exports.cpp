#include "exports.h"
#include <string>

extern "C" {
    int Add(int a, int b)
    {
        return a + b;
    }

    const char* GetMessagee()
    {
        static std::string message = "Hello from C++ via dll or .so";
        return message.c_str();
    }
}
