package com.fy.navi.patacnetlib.request.navibean.activate;


import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class UuidRequest {
    private String mApiVersion;
    private String mAppId;
    private String mSysVersion;
    private String mDeviceId;

    public UuidRequest(final String apiVersion, final String appId, final String sysVersion, final String deviceId) {
        this.mApiVersion = apiVersion;
        this.mAppId = appId;
        this.mSysVersion = sysVersion;
        this.mDeviceId = deviceId;
    }
}
