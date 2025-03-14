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

#include "vt_description.h"
#include "vtlog.h"

namespace vt {

VTJDescription::VTJDescription()
    : m_height(0)
    , m_width(0)
    , m_videoFormat(0) {
    LOGD("VTJDescription::VTJDescription()");
}

VTJDescription::~VTJDescription() {
    LOGD("VTJDescription::~VTJDescription()");
}

void VTJDescription::SetHeight(VTINT32 height) {
    LOGD("VTJDescription::SetHeight(), height: [%d]", height);
    m_height = height;
}

VTINT32 VTJDescription::GetHeight() const {
    LOGD("VTJDescription::GetHeight(), height: [%d]", m_height);
    return m_height;
}

void VTJDescription::SetWidth(VTINT32 width) {
    LOGD("VTJDescription::SetWidth(), width: [%d]", width);
    m_width = width;
}

VTINT32 VTJDescription::GetWidth() const {
    LOGD("VTJDescription::GetWidth(), width: [%d]", m_width);
    return m_width;
}

void VTJDescription::SetVideoFormat(VTINT32 format) {
    LOGD("VTJDescription::SetVideoFormat(), format: [%d]", format);
    m_videoFormat = format;
}

VTINT32 VTJDescription::GetVideoFormat() const {
    LOGD("VTJDescription::GetVideoFormat(), format: [%d]", m_videoFormat);
    return m_videoFormat;
}

} // namespace vt
