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

#include <gmock/gmock.h>
#include "VTTypes.h"
#include "VTServer.h"

namespace vt {

class MockVTSource : public IVTSource {
public:
    virtual ~MockVTSource() {}
    MOCK_METHOD0(Open, VTINT32());
    MOCK_METHOD0(Close, VTINT32());
    MOCK_METHOD1(GetDescription, VTVOID(IVTSourceDescription* desc));
    MOCK_METHOD1(Start, VTINT32(IVTSourceCallback* cb));
    MOCK_METHOD0(Stop, VTINT32());
};
}  /* namespace vt */
/* EOF */
