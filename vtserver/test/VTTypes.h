/**
 * Copyright @ 2020 - 2020 iAUTO(Shanghai) Co., Ltd.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are NOT permitted except as agreed by
 * iAUTO(Shanghai) Co., Ltd.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */

#pragma once

#include <stdint.h>

namespace vt {

// Basic types define
typedef void            VTVOID;
typedef char            VTCHAR;
typedef bool            VTBOOL;
typedef int             VTINT;
typedef short           VTSHORT;
typedef long            VTLONG;
typedef float           VTFLOAT;
typedef double          VTDOUBLE;
typedef unsigned char   VTBYTE;
typedef unsigned int    VTUINT32;
typedef unsigned short  VTUSHORT;
typedef unsigned long   VTULONG;
typedef int8_t          VTINT8;
typedef int16_t         VTINT16;
typedef int32_t         VTINT32;
typedef int64_t         VTINT64;
typedef uint8_t         VTUINT8;
typedef uint16_t        VTUINT16;
typedef uint32_t        VTUINT32;
typedef uint64_t        VTUINT64;

typedef void*           VTPVOID;
typedef char*           VTPCHAR;
typedef int*            VTPINT;
typedef short*          VTPSHORT;
typedef long*           VTPLONG;
typedef float*          VTPFLOAT;
typedef double*         VTPDOUBLE;
typedef unsigned char*  VTPBYTE;
typedef unsigned int*   VTPUINT;
typedef unsigned short* VTPUSHORT;
typedef unsigned long*  VTPULONG;
typedef int8_t*         VTPINT8;
typedef int16_t*        VTPINT16;
typedef int32_t*        VTPINT32;
typedef int64_t*        VTPINT64;
typedef uint8_t*        VTPUINT8;
typedef uint16_t*       VTPUINT16;
typedef uint32_t*       VTPUINT32;
typedef uint64_t*       VTPUINT64;


const VTINT32 VT_SUCCESS                    = 0;        // success
const VTINT32 VT_FAILURE                    = 1;        // failure
const VTINT32 VT_CLI_NOT_CONNECT            = 2;        // client not connect
const VTINT32 VT_SRC_NOT_FOUND              = 3;        // Source Not Found
const VTINT32 VT_CLI_NAME_DUPLICATED        = 4;        // Client Name Duplicated
const VTINT32 VT_CLI_ID_NOT_EXIST           = 5;        // Client ID Not Exist
const VTINT32 VT_SRC_HEIGHT_NOT_AVL         = 6;        // Source Height Not Available
const VTINT32 VT_SRC_WIDTH_NOT_AVL          = 7;        // Source Width Not Available
const VTINT32 VT_SRC_FORMAT_NOT_AVL         = 8;        // Source Format Not Available
const VTINT32 VT_CUS_INFO_FORMAT_INVALID    = 9;        // Custom Info Format Invalid
const VTINT32 VT_SRC_DATA_NOT_AVL           = 10;       // Source Data Not Available
const VTINT32 VT_SRC_DATA_TIMEOUT           = 11;       // Source Data Timeout
const VTINT32 VT_SRC_ABORT                  = 12;       // Source Abort

const VTPCHAR VT_ERROR_MSG[] = {
        (VTPCHAR)"success",
        (VTPCHAR)"failure",
        (VTPCHAR)"client not connect",
        (VTPCHAR)"Source Not Found",
        (VTPCHAR)"Client Name Duplicated",
        (VTPCHAR)"Client ID Not Exist",
        (VTPCHAR)"Source Height Not Available",
        (VTPCHAR)"Source Width Not Available",
        (VTPCHAR)"Source Format Not Available",
        (VTPCHAR)"Custom Info Format Invalid",
        (VTPCHAR)"Source Data Not Available",
        (VTPCHAR)"Source Data Timeout",
        (VTPCHAR)"Source Abort"
};

}  /* namespace vt */

/* EOF */
