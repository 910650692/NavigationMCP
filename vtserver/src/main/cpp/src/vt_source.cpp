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

#include "vt_source.h"
#include "vtlog.h"
#include <cstring>
#include <string>

namespace vt {

VTSource::VTSource()
    : m_source_cb(nullptr)
    , is_start(false) {
    LOGD("VTSource::VTSource()");
}

VTSource::~VTSource() {
    LOGD("VTSource::~VTSource()");
}

VTINT32 VTSource::Open() {
    LOGD("VTSource::Open()");
    return 0;
}

VTINT32 VTSource::Close() {
    LOGD("VTSource::Close()");
    return 0;
}

VTVOID VTSource::GetDescription(IVTSourceDescription *desc) {
    LOGD("VTSource::GetDescription()");
    desc->SetFormat(m_description.GetVideoFormat());
    desc->SetHeight(m_description.GetHeight());
    desc->SetWidth(m_description.GetWidth());
}

VTINT32 VTSource::Start(IVTSourceCallback *cb) {
    LOGD("VTSource::Start()");
    m_source_cb = cb;
    is_start = true;
    return 0;
}

VTINT32 VTSource::Stop() {
    LOGD("VTSource::Stop()");
    is_start = false;
    return 0;
}

VTVOID VTSource::SetDescription(const VTJDescription& description) {
    LOGD("VTSource::SetDescription()");
    m_description.SetWidth(description.GetWidth());
    m_description.SetHeight(description.GetHeight());
    m_description.SetVideoFormat(description.GetVideoFormat());
}

VTVOID VTSource::SetSourceData(VTBYTE* data, VTINT32 size) {
    LOGD("VTSource::SetSourceData(), size: [%d]", size);
    if (is_start) {
        m_source_cb->NotifySourceData(data, size);
    } else {
        LOGW("VTSource::SetSourceData(), source not start");
    }
}

VTVOID VTSource::SetError(VTINT32 code, const std::string& errMsg) {
    LOGD("VTSource::SetError(), code: [%d], errMsg: [%s]", code, errMsg.data());
    if (m_source_cb != nullptr) {
        m_source_cb->NotifyError(code);
    } else {
        LOGW("VTSource::SetError(), m_source_cb is null");
    }
}

} // namespace vt
