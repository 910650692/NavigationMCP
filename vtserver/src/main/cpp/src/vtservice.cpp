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
#include "vtlog.h"
#include "vtservice.h"
#include "VTServer.h"
#include "vtsource_factory.h"
#include "vt_source.h"
#include <iostream>

namespace vt {
    void VTService::MySaveBmp(const char *filename, unsigned char *rgbbuf, int width, int height)
    {
        MyBITMAPFILEHEADER bfh;
        MyBITMAPINFOHEADER bih;
        /* Magic number for file. It does not fit in the header structure due to alignment requirements, so put it outside */
        unsigned short bfType = 0x4d42;
        bfh.bfReserved1 = 0;
        bfh.bfReserved2 = 0;
        bfh.bfSize = sizeof(MyBITMAPFILEHEADER) + sizeof(MyBITMAPINFOHEADER) + width * height * 3;
        bfh.bfOffBits = 0x36;

        bih.biSize = sizeof(MyBITMAPINFOHEADER);
        bih.biWidth = width;
        bih.biHeight = -height;
        bih.biPlanes = 1;
        bih.biBitCount = 24;
        bih.biCompression = 0;
        bih.biSizeImage = 0;
        bih.biXPelsPerMeter = 5000;
        bih.biYPelsPerMeter = 5000;
        bih.biClrUsed = 0;
        bih.biClrImportant = 0;

        FILE *file = fopen(filename, "wb");
        int errNum = 0;
        if (!file)
        {
            errNum = errno;
            LOGD("VTService::NotifyVideoData(), Could not fopen file: [%s], errno: [%d], reason: [%s]", filename, errNum, strerror(errNum));
            return;
        }

        /*Write headers*/
        fwrite(&bfType, sizeof(bfType), 1, file);
        fwrite(&bfh, sizeof(bfh), 1, file);
        fwrite(&bih, sizeof(bih), 1, file);

        fwrite(rgbbuf, width*height * 3, 1, file);
        fclose(file);
    }

    void VTService::RGBAtoRGB(char *buf, long lenght) {
        char *prgba = buf;
        char *prgb = buf;
        for (long i = 0; i < lenght; i++, prgba++)
        {
            if ((i + 1) % 4 == 0 && i != 0)
                continue;
            *prgb++ = *prgba;
        }
    }

    void VTService::rgbaTobmp(const char *filename, unsigned char *rgbbuf, int width, int height)
    {
        // 因为传入的是rgba,所以*4
        RGBAtoRGB((char*)rgbbuf, height*width * 4);
        MySaveBmp(filename, rgbbuf, width, height);
    }

    VTService::VTService()
        : m_pServer(nullptr)
        , m_pSourceFac(nullptr) {
        LOGD("VTService::Constructor");
        m_pServer = CreateVTServer();
    }

    VTService::~VTService() {
        LOGD("VTService::~VTService()");
        if (m_pServer != nullptr) {
            DestroyVTServer(m_pServer);
            m_pServer = nullptr;
        }
        if (m_pSourceFac != nullptr) {
            delete m_pSourceFac;
            m_pSourceFac = nullptr;
        }
    }
    VTVOID VTService::SetJniCallBack(const std::shared_ptr<IVTServerListener>& callback) {
        LOGD("VTService::SetJniCallBack()");
        m_pServer->SetListener(callback.get());
    }

    VTINT32 VTService::Initialize() {
        LOGD("VTService::Initialize()");
        VTINT32 ret = VT_SUCCESS;
        if (m_pServer == nullptr) {
            LOGE("CreateServer Failed!!");
            ret = VT_FAILURE;
        } else {
            m_pServer->SetVTLog(VTLog::GetInstance());
            m_pSourceFac = new VTSourceFactory();
            m_pServer->SetVTSourceFactory(m_pSourceFac);
            ret = m_pServer->Initialize();
        }
        LOGD("End VTService::Initialize()");
        return ret;
    }

    VTINT32 VTService::Uninitialize() {
        LOGD("VTService::Uninitialize()");
        VTINT32 ret = VT_SUCCESS;
        if (m_pServer != nullptr) {
            m_pServer->Stop();
            ret = m_pServer->Uninitialize();
        } else {
            LOGE("VTService::Uninitialize() m_pServer is nullptr");
            ret = VT_FAILURE;
        }
        LOGD("End VTService::Uninitialize()");
        return ret;
    }

    VTVOID VTService::SetVideoDescription(const VTJDescription& description) {
        LOGD("VTService::SetVideoDescription()");
        m_pSourceFac->SetSourceDescription(description);
    }

    VTINT32 VTService::Start() {
        LOGD("Start VTService::Start()");
        VTINT32 ret = VT_SUCCESS;
        if (m_pServer != nullptr) {
            ret = m_pServer->Start();
        } else {
            LOGE("Start VTService::Start() m_pServer is nullptr");
            ret = VT_FAILURE;
        }
        LOGD("End VTService::Start()");
        return ret;
    }

    VTINT32 VTService::Stop() {
        LOGD("VTService::Stop()");
        VTINT32 ret = VT_SUCCESS;
        if (m_pServer != nullptr) {
            ret = m_pServer->Stop();
        } else {
            LOGE("VTService::Stop() m_pServer is nullptr");
            ret = VT_FAILURE;
        }
        LOGD("End VTService::Stop()");
        return ret;
    }

    VTVOID VTService::NotifyVideoData(VTBYTE* videoData, VTINT32 size) {
        LOGD("VTService::NotifyVideoData(), size: [%d]", size);
//        VTBYTE* adVideo = new VTBYTE[size];
//        // 复制数据到 adVideo
//        memcpy(adVideo, videoData, size);
//        std::string str;
//        str.assign((char*)adVideo, size);
//        LOGD("VTService::NotifyVideoData(), str.size: [%d]", str.size());
//        char imagePath[100] = {0};
//        sprintf(imagePath, "/storage/emulated/0/Android/data/com.fy.navi.hmi/cache/%d.bmp", m_imaggeCount++);
//        sprintf(imagePath, "/data/user/10/com.fy.navi.hmi/files/%d.bmp", m_imaggeCount++);
//        LOGD("VTService::NotifyVideoData(), imagePath: [%s]", imagePath);
//        rgbaTobmp(imagePath, adVideo, 328, 172);
        if (m_pSourceFac->GetSource() != nullptr) {
            m_pSourceFac->GetSource()->SetSourceData(videoData, size);
        }
    }

    VTVOID VTService::NotifyError(VTINT32 code, const std::string& errMsg) {
        LOGD("VTService::NotifyError(), code: [%d], errMsg: [%s]", code, errMsg.data());
        if (m_pSourceFac->GetSource() != nullptr) {
            m_pSourceFac->GetSource()->SetError(code, errMsg);
        }
    }
} // namespace vt