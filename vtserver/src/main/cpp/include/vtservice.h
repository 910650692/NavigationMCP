/**
 * Copyright @ 2020 - 2021 iAUTO(Shanghai) Co., Ltd.
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

#include "VTTypes.h"
#include "vt_description.h"
#include <string>

namespace vt {
    //BMP文件头（14字节）
    typedef struct                       /**** BMP file header structure ****/
    {
        unsigned int   bfSize;           /* Size of file */
        unsigned short bfReserved1;      /* Reserved */
        unsigned short bfReserved2;      /* ... */
        unsigned int   bfOffBits;        /* Offset to bitmap data */
    } MyBITMAPFILEHEADER;

    //位图信息头（40字节）
    typedef struct                       /**** BMP file info structure ****/
    {
        unsigned int   biSize;           /* Size of info header */
        int            biWidth;          /* Width of image */
        int            biHeight;         /* Height of image */
        unsigned short biPlanes;         /* Number of color planes */
        unsigned short biBitCount;       /* Number of bits per pixel */
        unsigned int   biCompression;    /* Type of compression to use */
        unsigned int   biSizeImage;      /* Size of image data */
        int            biXPelsPerMeter;  /* X pixels per meter */
        int            biYPelsPerMeter;  /* Y pixels per meter */
        unsigned int   biClrUsed;        /* Number of colors used */
        unsigned int   biClrImportant;   /* Number of important colors */
    } MyBITMAPINFOHEADER;


    class IVTServer;
    class VTSourceFactory;
    class VTSource;
    class IVTServerListener;
    class VTService {
    public:
        VTService();
        ~VTService();

        VTINT32 Initialize();
        VTINT32 Uninitialize();
        VTINT32 Start();
        VTINT32 Stop();

        VTVOID SetVideoDescription(const VTJDescription& description);
        VTVOID NotifyVideoData(VTBYTE* videoData, VTINT32 size);
        VTVOID NotifyError(VTINT32 code, const std::string& errMsg);
        VTVOID SetJniCallBack(const std::shared_ptr<IVTServerListener>& callback);
    private:
        void MySaveBmp(const char *filename, unsigned char *rgbbuf, int width, int height);
        void RGBAtoRGB(char *buf, long lenght);
        void rgbaTobmp(const char *filename, unsigned char *rgbbuf, int width, int height);
        int m_imaggeCount = 1;
        IVTServer* m_pServer;
        VTSourceFactory* m_pSourceFac;
    };
} // namespace vt