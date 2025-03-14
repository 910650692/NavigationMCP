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

#include "vtsource_factory.h"
#include "vt_source.h"
#include "vtlog.h"
#include "cstring"

namespace vt {

VTSourceFactory::VTSourceFactory()
    : m_pSource(nullptr){
    LOGD("VTSourceFactory::VTSourceFactory()");
}

VTSourceFactory::~VTSourceFactory() {
    LOGD("VTSourceFactory::~VTSourceFactory()");
    if (nullptr != m_pSource) {
        delete m_pSource;
        m_pSource = nullptr;
    }
}

IVTSource* VTSourceFactory::GetSource(const VTCHAR* srcName) {
    LOGI("VTSourceFactory::GetSource(%s)", srcName);
    if (strcmp(srcName, "roadnet") == 0) {
        m_pSource = new VTSource();
        m_pSource->SetDescription(m_description);
    }
    return m_pSource;
}

VTSource* VTSourceFactory::GetSource() {
    LOGI("VTSourceFactory::GetSource()");
    return m_pSource;
}
VTVOID VTSourceFactory::SetSourceDescription(const VTJDescription& description) {
    LOGI("VTSourceFactory::SetSourcedeDescription()");
    m_description.SetWidth(description.GetWidth());
    m_description.SetHeight(description.GetHeight());
    m_description.SetVideoFormat(description.GetVideoFormat());
}

} // namespace vt
