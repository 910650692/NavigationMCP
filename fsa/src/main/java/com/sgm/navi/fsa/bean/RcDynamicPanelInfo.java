package com.sgm.navi.fsa.bean;

/**
 * dynamicPanelInfos	ArrayList
 * <RCDynamicPanelInfo>	路况预测提示信息
 */
public class RcDynamicPanelInfo {
    /**
     * content	String	标题信息
     */
    private String content;
    /**
     * subContent	String	子标题信息
     */
    private String subContent;
    /**
     * displayDuration	int	展示时间
     */
    private int displayDuration;
    /**
     * pattern	int	模式
     */
    private int pattern;
    /**
     * iconId	int	图标Id
     */
    private int iconId;

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getSubContent() {
        return subContent;
    }

    public void setSubContent(String subContent) {
        this.subContent = subContent;
    }

    public int getDisplayDuration() {
        return displayDuration;
    }

    public void setDisplayDuration(int displayDuration) {
        this.displayDuration = displayDuration;
    }

    public int getPattern() {
        return pattern;
    }

    public void setPattern(int pattern) {
        this.pattern = pattern;
    }

    public int getIconId() {
        return iconId;
    }

    public void setIconId(int iconId) {
        this.iconId = iconId;
    }
}
