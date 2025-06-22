package com.sgm.navi.fsa.bean;

import java.util.ArrayList;
/**
 * 1.1.14、获取路况拥堵信息
 */
public class RcPredictionInfo {
    /**
     * notificationMsgs	ArrayList
     * <RCNotificationMsg>	路况拥堵提示信息
     */
    private ArrayList<RcNotificationMsg> notificationMsgs;
    /**
     * dynamicPanelInfos	ArrayList
     * <RCDynamicPanelInfo>	路况预测提示信息
     */
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
