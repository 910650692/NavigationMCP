//
// Created by hjs on 11/11/21.
//
#pragma once
#include <gmock/gmock.h>
#include "VTTypes.h"
#include "VTServer.h"

namespace vt {

    class MockVTSourceCallback  : public IVTSourceCallback {
    public:
        virtual ~MockVTSourceCallback() {}
        MOCK_METHOD2(NotifySourceData, VTVOID(const VTPBYTE data, VTUINT32 size));
        MOCK_METHOD1(NotifyError, VTVOID(VTINT32 code));
    };
}  /* namespace vt */
/* EOF */