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

#ifndef INCLUDE_VTSERVER_H_
#define INCLUDE_VTSERVER_H_

#include "VTTypes.h"

namespace vt {

enum EnVTLogLevel : VTINT32 {
    EN_VT_LOG_DEBUG = 0,
    EN_VT_LOG_INFO  = 1,
    EN_VT_LOG_WARN  = 2,
    EN_VT_LOG_ERROR = 3,
};

class IVTLog {
public:
    virtual ~IVTLog() {}
    virtual VTVOID Write(EnVTLogLevel level, const VTPCHAR msg) = 0;
};

class IVTDescription {
public:
    virtual ~IVTDescription() {}
    virtual VTVOID Add(const VTPCHAR key, const VTPCHAR value) = 0;
};

class IVTSourceDescription : public IVTDescription {
public:
    virtual ~IVTSourceDescription() {}
    virtual VTVOID SetWidth(VTINT32 width) = 0;
    virtual VTVOID SetHeight(VTINT32 height) = 0;
    virtual VTVOID SetFormat(VTINT32 format) = 0;
};

class IVTSourceCallback {
public:
    virtual ~IVTSourceCallback() {}
    virtual VTVOID NotifySourceData(const VTPBYTE data, VTUINT32 size) = 0;
    virtual VTVOID NotifyError(VTINT32 code) = 0;
};

class IVTSource {
public:
    virtual ~IVTSource() {}
    virtual VTINT32 Open() = 0;
    virtual VTINT32 Close() = 0;
    virtual VTVOID GetDescription(IVTSourceDescription* desc) = 0;
    virtual VTINT32 Start(IVTSourceCallback* cb) = 0;
    virtual VTINT32 Stop() = 0;
};

class IVTSourceFactory {
public:
    virtual ~IVTSourceFactory() {}
    virtual IVTSource* GetSource(const VTPCHAR src_name) = 0;
};

class IVTServer {
public:
    virtual ~IVTServer() {}
    virtual VTVOID SetVTLog(IVTLog* log) = 0;
    virtual VTVOID SetVTSourceFactory(IVTSourceFactory* src_factory) = 0;
    virtual VTINT32 Initialize() = 0;
    virtual VTINT32 Uninitialize() = 0;
    virtual VTINT32 Start() = 0;
    virtual VTINT32 Stop() = 0;
};

IVTServer* CreateVTServer(VTINT32 server_session_id, VTINT32 client_session_id);
VTVOID DestroyVTServer(IVTServer* server);

}  /* namespace vt */

#endif  /* INCLUDE_VTSERVER_H_ */

/* EOF */
