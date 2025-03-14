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
#include <thread>

namespace vt {
    class VTSource : public IVTSource {
    public:
        VTSource();
        virtual ~VTSource();
        virtual VTINT32 Open();
        virtual VTINT32 Close();
        virtual VTVOID GetDescription(IVTSourceDescription *desc);
        virtual VTINT32 Start(IVTSourceCallback *cb);
        virtual VTINT32 Stop();

        VTVOID SetDescription(const VTJDescription& description);
        VTVOID SetSourceData(VTBYTE* data, VTINT32 size);
        VTVOID SetError(VTINT32 code, const std::string& errMsg);

    private:
        IVTSourceCallback* m_source_cb;
        VTJDescription m_description;
        VTBOOL is_start;
    };
} // namespace vt
