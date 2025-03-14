package com.fy.navi.service.define.user.usertrack;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;


/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class HistoryRouteItemBean {
    public String itemId;
    public GeoPoint startLoc;
    public GeoPoint endLoc;
    public int method;
    public HistoryPoiItemBean fromPoi;
    public HistoryPoiItemBean toPoi;
    public ArrayList<HistoryPoiItemBean> midPoi;
    public long updateTime;

    public int type;
    public String id;

    public String getItemId() {
        return itemId;
    }

    public void setItemId(String itemId) {
        this.itemId = itemId;
    }

    public GeoPoint getStartLoc() {
        return startLoc;
    }

    public void setStartLoc(GeoPoint startLoc) {
        this.startLoc = startLoc;
    }

    public GeoPoint getEndLoc() {
        return endLoc;
    }

    public void setEndLoc(GeoPoint endLoc) {
        this.endLoc = endLoc;
    }

    public int getMethod() {
        return method;
    }

    public void setMethod(int method) {
        this.method = method;
    }

    public HistoryPoiItemBean getFromPoi() {
        return fromPoi;
    }

    public void setFromPoi(HistoryPoiItemBean fromPoi) {
        this.fromPoi = fromPoi;
    }

    public HistoryPoiItemBean getToPoi() {
        return toPoi;
    }

    public void setToPoi(HistoryPoiItemBean toPoi) {
        this.toPoi = toPoi;
    }

    public ArrayList<HistoryPoiItemBean> getMidPoi() {
        return midPoi;
    }

    public void setMidPoi(ArrayList<HistoryPoiItemBean> midPoi) {
        this.midPoi = midPoi;
    }

    public long getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(long updateTime) {
        this.updateTime = updateTime;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
