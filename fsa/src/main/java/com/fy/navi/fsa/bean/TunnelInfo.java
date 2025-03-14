package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class TunnelInfo {
    private boolean isTunnel;
    private ArrayList<DetectTunnelInfo> detectTunnelInfos;

    public boolean isTunnel() {
        return isTunnel;
    }

    public void setTunnel(boolean tunnel) {
        isTunnel = tunnel;
    }

    public ArrayList<DetectTunnelInfo> getDetectTunnelInfos() {
        return detectTunnelInfos;
    }

    public void setDetectTunnelInfos(ArrayList<DetectTunnelInfo> detectTunnelInfos) {
        this.detectTunnelInfos = detectTunnelInfos;
    }
}
