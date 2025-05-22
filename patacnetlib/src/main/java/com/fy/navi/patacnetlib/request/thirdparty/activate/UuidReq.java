package com.fy.navi.patacnetlib.request.thirdparty.activate;


import androidx.annotation.NonNull;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UuidReq extends SuperReq {

    @Header
    private String appId;
    @Header
    private String sysVersion;

    private String deviceId;

    @NonNull
    @Override
    public String toString() {
        return "UuidReq{" +
                "mAppId='" + appId + '\'' +
                ", sysVersion='" + sysVersion + '\'' +
                ", deviceId='" + deviceId + '\'' +
                '}';
    }

    public UuidReq(final String apiVersion, final String appId, final String sysVersion, final String deviceId) {
        super(apiVersion);
        this.appId = appId;
        this.sysVersion = sysVersion;
        this.deviceId = deviceId;
        setHeaderJson(true);
        setAddContentType(true);
    }

    public UuidReq(final String apiVersion, final String idpUserId, final String appId, final String sysVersion, final String deviceId) {
        super(apiVersion, idpUserId);
        this.appId = appId;
        this.sysVersion = sysVersion;
        this.deviceId = deviceId;
        setHeaderJson(true);
        setAddContentType(true);
    }
}
