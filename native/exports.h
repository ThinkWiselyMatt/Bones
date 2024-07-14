#pragma once

#if defined(__cplusplus)
extern "C" {
#endif

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __attribute__((visibility("default")))
#endif

EXPORT int Add(int a, int b);
EXPORT const char* GetMessagee(); //extra e intentional

#if defined(__cplusplus)
}
#endif
