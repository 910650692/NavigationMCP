package com.fy.navi.fsa.bean;

import java.util.ArrayList;

/**
 * 1.1.10、获取隧道信息
 */
public class TunnelInfo {
    /**
     * isTunnel	boolean	是否处于隧道
     * true – 处于隧道
     * false – 不处于隧道
     */
    private boolean isTunnel;
    /**
     * detectTunnelInfos	ArrayList<DetectTunnelInfo>    隧道信息
     */
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
