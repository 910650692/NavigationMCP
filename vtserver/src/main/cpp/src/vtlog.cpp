
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
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdarg.h>
#include <sys/time.h>
#include <android/log.h>
namespace vt {
#define LOG_TIME_FORMAT "%d-%02d-%02d %02d:%02d:%02d:[%06ld]"
    static std::string getTimeString() {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        time_t sec = tv.tv_sec;
        struct tm cur_tm;
        localtime_r((time_t*)&sec, &cur_tm);
        char cur_time[40] = { 0 };
        snprintf(cur_time, 30, LOG_TIME_FORMAT, cur_tm.tm_year + 1900, cur_tm.tm_mon + 1, cur_tm.tm_mday, cur_tm.tm_hour, cur_tm.tm_min, cur_tm.tm_sec, tv.tv_usec);
        return std::string(cur_time);
    }
    VTLog* VTLog::s_instance = nullptr;
    VTLog* VTLog::GetInstance() {
        if (s_instance == nullptr) {
            s_instance = new VTLog();
        }
        return s_instance;
    }
    VTVOID VTLog::DelInstance() {
        if (s_instance != nullptr) {
            delete s_instance;
            s_instance = nullptr;
        }
    }
    VTLog::VTLog() {
    }
    VTLog::~VTLog() {
    }
    VTVOID VTLog::Write(EnVTLogLevel level, const VTCHAR* msg) {
        Write(level, "vtserver-jni", msg);
    }
    VTVOID VTLog::Write(EnVTLogLevel level, const std::string& sfile, const VTCHAR* msg) {
#ifndef TEST
        int android_level = ANDROID_LOG_UNKNOWN;
        switch (level)
        {
            case vt::EnVTLogLevel::EN_VT_LOG_DEBUG:
                android_level = ANDROID_LOG_DEBUG;
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_INFO:
                android_level = ANDROID_LOG_INFO;
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_WARN:
                android_level = ANDROID_LOG_WARN;
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_ERROR:
                android_level = ANDROID_LOG_ERROR;
                break;
            default:
                break;
        }
        __android_log_write(android_level,sfile.data(),msg);

#else
        if (!msg) {
            return;
        }
        std::string level_label = "UNKOWN";
        switch (level)
        {
            case vt::EnVTLogLevel::EN_VT_LOG_DEBUG:
                level_label = "DEBUG";
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_INFO:
                level_label = "INFO";
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_WARN:
                level_label = "WARN";
                break;
            case vt::EnVTLogLevel::EN_VT_LOG_ERROR:
                level_label = "ERROR";
                break;
            default:
                break;
        }
        std::ostringstream oss;
        std::string time = getTimeString();
        oss << std::left << std::setw(16) << time << " ";
        oss << " [" << std::setw(5) << level_label << "]";
        oss << " " << "[" << sfile << "]";
        oss << " " << msg;
        oss << std::endl;
        std::cout << oss.str().c_str();
#endif
    }
} // namespace vt
