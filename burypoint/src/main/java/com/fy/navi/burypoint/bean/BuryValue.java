package com.fy.navi.burypoint.bean;

public class BuryValue {

    // 消息ID（由SDK生成）
    private String mid;

    // 设备ID
    //    统一服务：vin
    //    独立APP：SDK采集的设备id
    private String did;

    // 应用ID/BasicAuth账号（set方法）
    private String aid;

    // 数据模板ID（传入）
    private String sid;

    // 数据模板版本ID（传入）
    private String svid;

    // 数据采集时间戳（由SDK生成）
    private String ts;

    // 采集数据
    private BuryProperty property;

    public String getMid() {
        return mid;
    }

    public void setMid(String mid) {
        this.mid = mid;
    }

    public String getDid() {
        return did;
    }

    public void setDid(String did) {
        this.did = did;
    }

    public String getAid() {
        return aid;
    }

    public void setAid(String aid) {
        this.aid = aid;
    }

    public String getSid() {
        return sid;
    }

    public void setSid(String sid) {
        this.sid = sid;
    }

    public String getSvid() {
        return svid;
    }

    public void setSvid(String svid) {
        this.svid = svid;
    }

    public String getTs() {
        return ts;
    }

    public void setTs(String ts) {
        this.ts = ts;
    }

    public BuryProperty getProperty() {
        return property;
    }

    public void setProperty(BuryProperty property) {
        this.property = property;
    }
}
