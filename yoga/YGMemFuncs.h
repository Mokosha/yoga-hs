#pragma once
#include "YGMacros.h"

YG_EXTERN_C_BEGIN

typedef void *(*YGMalloc)(size_t size);
typedef void *(*YGCalloc)(size_t count, size_t size);
typedef void *(*YGRealloc)(void *ptr, size_t size);
typedef void (*YGFree)(void *ptr);

WIN_EXPORT YGMalloc YGGetMalloc();
WIN_EXPORT YGCalloc YGGetCalloc();
WIN_EXPORT YGRealloc YGGetRealloc();
WIN_EXPORT YGFree  YGGetFree();

YG_EXTERN_C_END
