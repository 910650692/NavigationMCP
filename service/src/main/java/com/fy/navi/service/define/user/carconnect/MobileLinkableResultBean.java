package com.fy.navi.service.define.user.carconnect;

import java.util.ArrayList;

public class MobileLinkableResultBean extends TaskResultBean{

    private String lottieUrl = "";
    private ArrayList<MobileLinkableDeviceBean> devices = new ArrayList<>();

    public String getLottieUrl() {
        return lottieUrl;
    }

    public void setLottieUrl(String lottieUrl) {
        this.lottieUrl = lottieUrl;
    }

    public ArrayList<MobileLinkableDeviceBean> getDevices() {
        return devices;
    }

    public void setDevices(ArrayList<MobileLinkableDeviceBean> devices) {
        this.devices = devices;
    }
}
