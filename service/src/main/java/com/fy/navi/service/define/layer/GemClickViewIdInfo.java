package com.fy.navi.service.define.layer;

import java.io.Serializable;

/**
 * Author: QiuYaWei
 * Date: 2025/2/8
 * Description: [点击View的id信息，参照：ClickViewIdInfo]
 */
public class GemClickViewIdInfo implements Serializable {
    private String poiMarkerClickViewId;
    private String bgMarkerClickViewId;
    private String bubbleMarkerClickViewId;

    public GemClickViewIdInfo() {
        this.poiMarkerClickViewId = "";
        this.bgMarkerClickViewId = "";
        this.bubbleMarkerClickViewId = "";
    }

    public GemClickViewIdInfo(String poiMarkerClickViewIdLiteObj, String bgMarkerClickViewIdLiteObj, String bubbleMarkerClickViewIdLiteObj) {
        this.poiMarkerClickViewId = poiMarkerClickViewIdLiteObj;
        this.bgMarkerClickViewId = bgMarkerClickViewIdLiteObj;
        this.bubbleMarkerClickViewId = bubbleMarkerClickViewIdLiteObj;
    }

    public String getPoiMarkerClickViewId() {
        return poiMarkerClickViewId;
    }

    public void setPoiMarkerClickViewId(String poiMarkerClickViewId) {
        this.poiMarkerClickViewId = poiMarkerClickViewId;
    }

    public String getBgMarkerClickViewId() {
        return bgMarkerClickViewId;
    }

    public void setBgMarkerClickViewId(String bgMarkerClickViewId) {
        this.bgMarkerClickViewId = bgMarkerClickViewId;
    }

    public String getBubbleMarkerClickViewId() {
        return bubbleMarkerClickViewId;
    }

    public void setBubbleMarkerClickViewId(String bubbleMarkerClickViewId) {
        this.bubbleMarkerClickViewId = bubbleMarkerClickViewId;
    }
}
