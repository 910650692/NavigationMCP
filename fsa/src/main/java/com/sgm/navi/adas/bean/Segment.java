package com.sgm.navi.adas.bean;

class Segment {
    private String description;
    private boolean isArriveWayPoint;
    private int navigationAssitAction;
    private int navigationLen;
    private int navigationMainAction;
    private String navigationNextRoadName;

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isArriveWayPoint() {
        return isArriveWayPoint;
    }

    public void setArriveWayPoint(boolean arriveWayPoint) {
        isArriveWayPoint = arriveWayPoint;
    }

    public int getNavigationAssitAction() {
        return navigationAssitAction;
    }

    public void setNavigationAssitAction(int navigationAssitAction) {
        this.navigationAssitAction = navigationAssitAction;
    }

    public int getNavigationLen() {
        return navigationLen;
    }

    public void setNavigationLen(int navigationLen) {
        this.navigationLen = navigationLen;
    }

    public int getNavigationMainAction() {
        return navigationMainAction;
    }

    public void setNavigationMainAction(int navigationMainAction) {
        this.navigationMainAction = navigationMainAction;
    }

    public String getNavigationNextRoadName() {
        return navigationNextRoadName;
    }

    public void setNavigationNextRoadName(String navigationNextRoadName) {
        this.navigationNextRoadName = navigationNextRoadName;
    }
}
