package com.fy.navi.fsa.bean;

public class RcDynamicPanelInfo {
    private String content;
    private String subContent;
    private int displayDuration;
    private int pattern;
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
