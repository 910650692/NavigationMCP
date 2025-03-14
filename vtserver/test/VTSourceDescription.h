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

#include "VTTypes.h"
#include "VTServer.h"

namespace vt {

class VTSourceDescription : public IVTSourceDescription {
public:
    virtual ~VTSourceDescription() {}
    virtual VTVOID SetWidth(VTINT32 width) {m_width = width;}
    virtual VTVOID SetHeight(VTINT32 height) {m_height = height;}
    virtual VTVOID SetFormat(VTINT32 format) {m_format = format;}
    virtual VTVOID Add(const VTPCHAR key, const VTPCHAR value) {}
    const VTINT32 GetWidth() {return m_width;}
    const VTINT32 GetHeight() {return m_height;}
    const VTINT32 GetFormat() {return m_format;}

private:
    VTINT32 m_width;
    VTINT32 m_height;
    VTINT32 m_format;
};
}  /* namespace vt */