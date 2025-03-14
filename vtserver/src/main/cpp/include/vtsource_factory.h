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
#include "vt_description.h"

namespace vt {
    class VTSource;
    class VTSourceFactory: public IVTSourceFactory {
    public:
        VTSourceFactory();
        virtual ~VTSourceFactory();
        virtual IVTSource* GetSource(const VTCHAR* srcName);

        VTVOID SetSourceDescription(const VTJDescription& description);
        VTSource* GetSource();
    private:
        VTSource* m_pSource;
        VTJDescription m_description;
    };
} // namespace vt
