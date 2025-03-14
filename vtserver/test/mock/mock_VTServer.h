//
// Created by hjs on 11/11/21.
//
#pragma once
#include <gmock/gmock.h>
#include "VTTypes.h"
#include "VTServer.h"

namespace vt {

    class MockVTServer : public IVTServer {
    public:
        MockVTServer(VTINT32 server_session_id, VTINT32 client_session_id) {}
        virtual ~MockVTServer() {}
        MOCK_METHOD1(SetVTLog, VTVOID(IVTLog* log));
        MOCK_METHOD1(SetVTSourceFactory, VTVOID(IVTSourceFactory* src_factory));
        MOCK_METHOD0(Initialize, VTINT32());
        MOCK_METHOD0(Uninitialize, VTINT32());
        MOCK_METHOD0(Start, VTINT32());
        MOCK_METHOD0(Stop, VTINT32());
    };
IVTServer* CreateVTServer(VTINT32 server_session_id, VTINT32 client_session_id) {
    IVTServer* server = new MockVTServer(server_session_id, client_session_id);
    return server;
};
VTVOID DestroyVTServer(IVTServer* server) {
    delete server;
    server = nullptr;
};
}  /* namespace vt */
/* EOF */
