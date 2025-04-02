package com.fy.navi.service.define.user.carconnect;

import java.util.ArrayList;

public class MobileLinkableResultBean extends TaskResultBean{

    private String mLottieUrl = "";
    private ArrayList<MobileLinkableDeviceBean> mDevices = new ArrayList<>();

    public String getLottieUrl() {
        return mLottieUrl;
    }

    public void setLottieUrl(final String lottieUrl) {
        this.mLottieUrl = lottieUrl;
    }

    public ArrayList<MobileLinkableDeviceBean> getDevices() {
        return mDevices;
    }

    public void setDevices(final ArrayList<MobileLinkableDeviceBean> devices) {
        this.mDevices = devices;
    }
}
