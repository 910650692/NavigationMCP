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
typedef unsigned int    VTUINT;
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


enum EnVTErrorMsg : VTINT32 {
    VT_SUCCESS                    = 0,        // success
    VT_FAILURE                    = 1,        // failure
    VT_CLI_NOT_CONNECT            = 2,        // client not connect
    VT_SRC_NOT_FOUND              = 3,        // Source Not Found
    VT_CLI_NAME_DUPLICATED        = 4,        // Client Name Duplicated
    VT_CLI_ID_NOT_EXIST           = 5,        // Client ID Not Exist
    VT_SRC_HEIGHT_NOT_AVL         = 6,        // Source Height Not Available
    VT_SRC_WIDTH_NOT_AVL          = 7,        // Source Width Not Available
    VT_SRC_FORMAT_NOT_AVL         = 8,        // Source Format Not Available
    VT_CUS_INFO_FORMAT_INVALID    = 9,        // Custom Info Format Invalid
    VT_SRC_DATA_NOT_AVL           = 10,       // Source Data Not Available
    VT_SRC_DATA_TIMEOUT           = 11,       // Source Data Timeout
    VT_SRC_ABORT                  = 12,       // Source Abort
    VT_CLI_CONNECT_LOST           = 13,       // Client connection lost
};

}  /* namespace vt */

/* EOF */
