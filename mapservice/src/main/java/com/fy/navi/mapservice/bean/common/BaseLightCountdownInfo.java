package com.fy.navi.mapservice.bean.common;

//红绿灯倒计时信息
public class BaseLightCountdownInfo {

    //路线ID
    private long mPathID;
    //路况状态 @range [ 1: 极度拥堵, 2: 拥堵, 3: 拥堵+缓行, 4: 缓行, 5: 畅通 ]
    private int mStatus;
    //所在linkID值
    private long mLinkID;
    //segment导航段索引
    private long mSegmentIndex;
    //link段索引
    private long mLinkIndex;
    //所在位置
    private BaseGeoPoint mPosition;
    //实时红绿灯信息
    private BaseLightInfo mLightInfo;

    public long getPathID() {
        return mPathID;
    }

    public void setPathID(final long pathID) {
        mPathID = pathID;
    }

    public int getStatus() {
        return mStatus;
    }

    public void setStatus(final int status) {
        mStatus = status;
    }

    public long getLinkID() {
        return mLinkID;
    }

    public void setLinkID(final long linkID) {
        mLinkID = linkID;
    }

    public long getSegmentIndex() {
        return mSegmentIndex;
    }

    public void setSegmentIndex(final long segmentIndex) {
        mSegmentIndex = segmentIndex;
    }

    public long getLinkIndex() {
        return mLinkIndex;
    }

    public void setLinkIndex(final long linkIndex) {
        mLinkIndex = linkIndex;
    }

    public BaseGeoPoint getPosition() {
        return mPosition;
    }

    public void setPosition(final BaseGeoPoint position) {
        mPosition = position;
    }

    public BaseLightInfo getLightInfo() {
        return mLightInfo;
    }

    public void setLightInfo(final BaseLightInfo lightInfo) {
        mLightInfo = lightInfo;
    }

}
