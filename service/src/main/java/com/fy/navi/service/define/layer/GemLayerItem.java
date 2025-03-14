package com.fy.navi.service.define.layer;

import java.io.Serializable;

/**
 * Author: QiuYaWei
 * Date: 2025/2/8
 * Description: [图层实体类，参照 LayerItem]
 */
public class GemLayerItem implements Serializable {
    // 设置默认类型为未知
    private GemLayerClickBusinessType clickBusinessType = GemLayerClickBusinessType.UnKnown;
    // 下标
    private long layerItemId;
    private double log;
    private double lat;
    private double z;
    // 事件id
    private long eventId;
    private GemClickViewIdInfo clickViewIdInfo;

    public long getIndex() {
        return layerItemId;
    }

    public void setLayerItemId(long index) {
        this.layerItemId = index;
    }

    public double getLog() {
        return log;
    }

    public void setLog(double log) {
        this.log = log;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public double getZ() {
        return z;
    }

    public void setZ(double z) {
        this.z = z;
    }

    public long getEventId() {
        return eventId;
    }

    public void setEventId(long eventId) {
        this.eventId = eventId;
    }

    public GemClickViewIdInfo getClickViewIdInfo() {
        return clickViewIdInfo;
    }

    public void setClickViewIdInfo(GemClickViewIdInfo clickViewIdInfo) {
        this.clickViewIdInfo = clickViewIdInfo;
    }

    public GemLayerClickBusinessType getClickBusinessType() {
        return clickBusinessType;
    }

    public void setClickBusinessType(GemLayerClickBusinessType clickBusinessType) {
        this.clickBusinessType = clickBusinessType;
    }
}
