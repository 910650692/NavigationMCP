package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class RcPredictionInfo {
    private ArrayList<RcNotificationMsg> notificationMsgs;
    private ArrayList<RcDynamicPanelInfo> dynamicPanelInfos;

    public ArrayList<RcNotificationMsg> getNotificationMsgs() {
        return notificationMsgs;
    }

    public void setNotificationMsgs(ArrayList<RcNotificationMsg> notificationMsgs) {
        this.notificationMsgs = notificationMsgs;
    }

    public ArrayList<RcDynamicPanelInfo> getDynamicPanelInfos() {
        return dynamicPanelInfos;
    }

    public void setDynamicPanelInfos(ArrayList<RcDynamicPanelInfo> dynamicPanelInfos) {
        this.dynamicPanelInfos = dynamicPanelInfos;
    }
}
