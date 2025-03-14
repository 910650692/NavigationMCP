#pragma once

#include <gmock/gmock.h>
#include "VTTypes.h"
#include "VTServer.h"

namespace vt {

class MockVTSourceFactory : public IVTSourceFactory {
public:
    virtual ~MockVTSourceFactory() {}
    MOCK_METHOD1(GetSource, IVTSource*(const VTPCHAR src_name));
};
}  /* namespace vt */
/* EOF */
