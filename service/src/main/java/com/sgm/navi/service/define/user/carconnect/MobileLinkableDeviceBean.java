package com.sgm.navi.service.define.user.carconnect;

public class MobileLinkableDeviceBean {
    private String manufacture;
    private long mDeviceId;

    public String getManufacture() {
        return manufacture;
    }

    public void setManufacture(final String manufacture) {
        this.manufacture = manufacture;
    }

    public long getDeviceId() {
        return mDeviceId;
    }

    public void setDeviceId(final long deviceId) {
        this.mDeviceId = deviceId;
    }
}
