package com.sgm.navi.service.define.layer.refix;

import lombok.Getter;
import lombok.Setter;

/**
 * 路线图层点击返回bean类
 */
@Setter
@Getter
public class LayerItemRoutePointClickResult {

    // 下标
    private long index;
    private long eventID;
    private double log;
    private double lat;

    @Override
    public String toString() {
        return "LayerItemRoutePointClickResult{" +
                "index=" + index +
                ", eventID=" + eventID +
                ", log=" + log +
                ", lat=" + lat +
                '}';
    }
}
