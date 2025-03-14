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
#include "VTServer.h"
#include "vtlog.h"
#include "vtservice.h"
#include "mock_VTServer.h"
#include "mock_VTSource.h"
#include "mock_VTSourceFactory.h"

using namespace vt;

const VTINT32 SERVER_SESSION_ID = 1;
const VTINT32 CLIENT_SESSION_ID = 2;

class VTServiceTest : public ::testing::Test {
protected:

    // You can remove any or all of the following functions if their bodies
    // would be empty.
    VTServiceTest() {
        // You can do set-up work for each test here.
    }

    virtual ~VTServiceTest() {
    }

    // If the constructor and destructor are not enough for setting up
    // and cleaning up each test, you can define the following methods:
    void SetUp() override {
        // Code here will be called immediately after the constructor (right
        // before each test).
        m_pService = new VTService(SERVER_SESSION_ID, CLIENT_SESSION_ID);
        LOGD("VTServiceTest::VTServiceTest()");
//        m_pService->m_pServer = new MockVTServer(SERVER_SESSION_ID, CLIENT_SESSION_ID);
    }

    void TearDown() override {
        // Code here will be called immediately after each test (right
        // before the destructor).
        LOGD("VTServiceTest::TearDown()");
        if (nullptr != m_pService) {
            delete m_pService;
            m_pService = nullptr;
        }
    }
    VTService* m_pService;
};

TEST_F(VTServiceTest, Initialize_OK) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Initialize()).Times(1).WillOnce(testing::Return(VT_SUCCESS));
    EXPECT_EQ(m_pService->Initialize(), VT_SUCCESS);
}

TEST_F(VTServiceTest, Initialize_Failed) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Initialize()).Times(1).WillOnce(testing::Return(VT_FAILURE));
    EXPECT_EQ(m_pService->Initialize(), VT_FAILURE);
}

TEST_F(VTServiceTest, Start_OK) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Start()).Times(1).WillOnce(testing::Return(VT_SUCCESS));
    EXPECT_EQ(m_pService->Start(), VT_SUCCESS);
}

TEST_F(VTServiceTest, Start_Failed) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Start()).Times(1).WillOnce(testing::Return(VT_FAILURE));
    EXPECT_EQ(m_pService->Start(), VT_FAILURE);
}

TEST_F(VTServiceTest, Stop_OK) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Stop()).Times(1).WillOnce(testing::Return(VT_SUCCESS));
    EXPECT_EQ(m_pService->Stop(), VT_SUCCESS);
}

TEST_F(VTServiceTest, Stop_Failed) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Stop()).Times(1).WillOnce(testing::Return(VT_FAILURE));
    EXPECT_EQ(m_pService->Stop(), VT_FAILURE);
}

TEST_F(VTServiceTest, Uninitialize_OK) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Uninitialize()).Times(1).WillOnce(testing::Return(VT_SUCCESS));
    EXPECT_EQ(m_pService->Uninitialize(), VT_SUCCESS);
}

TEST_F(VTServiceTest, Uninitialize_Failed) {
    EXPECT_CALL(*((MockVTServer*)m_pService->m_pServer), Uninitialize()).Times(1).WillOnce(testing::Return(VT_FAILURE));
    EXPECT_EQ(m_pService->Uninitialize(), VT_FAILURE);
}