package com.sgm.navi.fsa.bean;

/**
 * notificationMsgs	ArrayList
 * <RCNotificationMsg>	路况拥堵提示信息
 */
public class RcNotificationMsg {
    /**
     * title	String	事件标题
     */
    private String title;
    /**
     * subTitle	String	事件副标题
     */
    private String subTitle;
    /**
     * assistInfo	String	富文本文字，用于纠错和本地化类型
     */
    private String assistInfo;
    /**
     * tipCopy	String	小度语音个性化播报文案
     */
    private String tipCopy;
    /**
     * tipBroad	String	导航直跳播报文案
     */
    private String tipBroad;
    /**
     * permitId	String	非车牌限行ID
     */
    private String permitId;
    /**
     * tipId	int	小黄条唯一标识ID
     */
    private int tipId;
    /**
     * iconId	int	云端干预特定图片的Id序列
     */
    private int iconId;
    /**
     * backColorId	int	背景颜色
     */
    private int backColorId;
    /**
     * remindTime	int	路口预测加入行程助手的提醒时间
     */
    private int remindTime;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getSubTitle() {
        return subTitle;
    }

    public void setSubTitle(String subTitle) {
        this.subTitle = subTitle;
    }

    public String getAssistInfo() {
        return assistInfo;
    }

    public void setAssistInfo(String assistInfo) {
        this.assistInfo = assistInfo;
    }

    public String getTipCopy() {
        return tipCopy;
    }

    public void setTipCopy(String tipCopy) {
        this.tipCopy = tipCopy;
    }

    public String getTipBroad() {
        return tipBroad;
    }

    public void setTipBroad(String tipBroad) {
        this.tipBroad = tipBroad;
    }

    public String getPermitId() {
        return permitId;
    }

    public void setPermitId(String permitId) {
        this.permitId = permitId;
    }

    public int getTipId() {
        return tipId;
    }

    public void setTipId(int tipId) {
        this.tipId = tipId;
    }

    public int getIconId() {
        return iconId;
    }

    public void setIconId(int iconId) {
        this.iconId = iconId;
    }

    public int getBackColorId() {
        return backColorId;
    }

    public void setBackColorId(int backColorId) {
        this.backColorId = backColorId;
    }

    public int getRemindTime() {
        return remindTime;
    }

    public void setRemindTime(int remindTime) {
        this.remindTime = remindTime;
    }
}
