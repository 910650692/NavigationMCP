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

#include <iostream>
#include "gtest/gtest.h"
#define private public
#define protected public
#include "VTTypes.h"
#include "VTSourceDescription.h"
#include "vtlog.h"
#include "vt_source.h"
#include "mock_VTSourceCallback.h"

using namespace vt;

class VTSourceTest : public ::testing::Test {
protected:

    // You can remove any or all of the following functions if their bodies
    // would be empty.
    VTSourceTest() {
        // You can do set-up work for each test here.
    }

    virtual ~VTSourceTest() {
    }

    // If the constructor and destructor are not enough for setting up
    // and cleaning up each test, you can define the following methods:
    void SetUp() override {
        // Code here will be called immediately after the constructor (right
        // before each test).
        m_psource = new VTSource();
        m_callback = new MockVTSourceCallback();
    }

    void TearDown() override {
        // Code here will be called immediately after each test (right
        // before the destructor).
        if (nullptr != m_callback) {
            delete m_callback;
            m_callback = nullptr;
        }
        if (nullptr != m_psource) {
            delete m_psource;
            m_psource = nullptr;
        }
    }
    VTSource* m_psource;
    IVTSourceCallback* m_callback;
};

TEST_F(VTSourceTest, GetDescription) {
    VTJDescription description;
    description.SetHeight(100);
    description.SetWidth(100);
    description.SetVideoFormat(100);
    m_psource->SetDescription(description);
    VTSourceDescription src_description;
    m_psource->GetDescription(&src_description);
    EXPECT_EQ(src_description.GetWidth(), description.GetWidth());
    EXPECT_EQ(src_description.GetHeight(), description.GetHeight());
    EXPECT_EQ(src_description.GetFormat(), description.GetVideoFormat());
}

TEST_F(VTSourceTest, SetSourceData) {
    VTPBYTE data = nullptr;
    VTINT32 size = 0;
    m_psource->Start(m_callback);
    EXPECT_CALL(*((MockVTSourceCallback*)m_psource->m_source_cb), NotifySourceData(testing::Eq(data),testing::Eq(size))).Times(1).WillOnce(testing::Return());
    m_psource->SetSourceData(data, 0);
}

TEST_F(VTSourceTest, SetError) {
    VTINT32 code = 0;
    std::string msg = "";
    m_psource->Start(m_callback);
    EXPECT_CALL(*((MockVTSourceCallback*)m_psource->m_source_cb), NotifyError(testing::Eq(code))).Times(1).WillOnce(testing::Return());
    m_psource->SetError(code, msg);
}