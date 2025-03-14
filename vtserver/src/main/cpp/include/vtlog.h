
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
#include "VTServer.h"
#include <libgen.h>
#include <string>
#define VT_LOG_BUFFER_SIZE 2048
#define LOG(level, format, args...) \
    { \
        char message_buf[VT_LOG_BUFFER_SIZE] = { 0 }; \
        std::string file_num_func = std::string(basename((char *)__FILE__)) \
        + std::string(":") + std::to_string(__LINE__) \
        + std::string(" ") + std::string(__FUNCTION__);  \
        snprintf(message_buf, VT_LOG_BUFFER_SIZE - 1, format, ##args);      \
        vt::VTLog::GetInstance()->Write(level, file_num_func, message_buf); \
    }
#define LOGD(format, args...) \
    { LOG(vt::EnVTLogLevel::EN_VT_LOG_DEBUG, format, ##args); }
#define LOGI(format, args...) \
    { LOG(vt::EnVTLogLevel::EN_VT_LOG_INFO, format, ##args); }
#define LOGW(format, args...) \
    { LOG(vt::EnVTLogLevel::EN_VT_LOG_WARN, format, ##args); }
#define LOGE(format, args...) \
    { LOG(vt::EnVTLogLevel::EN_VT_LOG_ERROR, format, ##args); }


namespace vt {
    class VTLog : public IVTLog
    {
    public:
        static VTLog* GetInstance();
        static VTVOID DelInstance();
        virtual VTVOID Write(EnVTLogLevel level, const VTCHAR* msg);
        VTVOID Write(EnVTLogLevel level, const std::string& sfile, const VTCHAR* msg);
    private:
        VTLog();
        virtual ~VTLog();
        static VTLog* s_instance;
    };
} // namespace vt

