package com.fy.navi.service.define.navi;

public class SoundInfoEntity {
    private String text;//播报文本
    private int ringType;//叮咚音类型
    private boolean isRingType = false;//是否播报叮咚音

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public int getRingType() {
        return ringType;
    }

    public void setRingType(int ringType) {
        this.ringType = ringType;
    }

    public boolean isRingType() {
        return isRingType;
    }

    public void setRingType(boolean ringType) {
        isRingType = ringType;
    }
}
