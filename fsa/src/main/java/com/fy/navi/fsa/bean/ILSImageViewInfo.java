package com.fy.navi.fsa.bean;

import java.util.Objects;

public class ILSImageViewInfo {
    private int type;
    private int width;
    private int height;
    private int remainDistance;
    private int progressRatio;
    private String arrowMapName;
    private String backgroundMapName;
    private String nextRoadName;
    private String backgroundMapBytes;
    private String arrowMapBytes;
    private String backgroundMimeType;
    private String arrowMimeType;

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getProgressRatio() {
        return progressRatio;
    }

    public void setProgressRatio(int progressRatio) {
        this.progressRatio = progressRatio;
    }

    public String getArrowMapName() {
        return arrowMapName;
    }

    public void setArrowMapName(String arrowMapName) {
        this.arrowMapName = arrowMapName;
    }

    public String getBackgroundMapName() {
        return backgroundMapName;
    }

    public void setBackgroundMapName(String backgroundMapName) {
        this.backgroundMapName = backgroundMapName;
    }

    public String getNextRoadName() {
        return nextRoadName;
    }

    public void setNextRoadName(String nextRoadName) {
        this.nextRoadName = nextRoadName;
    }

    public String getBackgroundMapBytes() {
        return backgroundMapBytes;
    }

    public void setBackgroundMapBytes(String backgroundMapBytes) {
        this.backgroundMapBytes = backgroundMapBytes;
    }

    public String getArrowMapBytes() {
        return arrowMapBytes;
    }

    public void setArrowMapBytes(String arrowMapBytes) {
        this.arrowMapBytes = arrowMapBytes;
    }

    public String getBackgroundMimeType() {
        return backgroundMimeType;
    }

    public void setBackgroundMimeType(String backgroundMimeType) {
        this.backgroundMimeType = backgroundMimeType;
    }

    public String getArrowMimeType() {
        return arrowMimeType;
    }

    public void setArrowMimeType(String arrowMimeType) {
        this.arrowMimeType = arrowMimeType;
    }

    @Override
    public String toString() {
        return "ILSImageViewInfo{" +
                "type=" + type +
                ", width=" + width +
                ", height=" + height +
                ", remainDistance=" + remainDistance +
                ", progressRatio=" + progressRatio +
                ", arrowMapName='" + arrowMapName + '\'' +
                ", backgroundMapName='" + backgroundMapName + '\'' +
                ", nextRoadName='" + nextRoadName + '\'' +
                ", backgroundMapBytes='" + (Objects.isNull(backgroundMapBytes) ? "null" : backgroundMapBytes.length()) + '\'' +
                ", arrowMapBytes='" + (Objects.isNull(arrowMapBytes) ? "null" : arrowMapBytes.length()) + '\'' +
                ", backgroundMimeType='" + backgroundMimeType + '\'' +
                ", arrowMimeType='" + arrowMimeType + '\'' +
                '}';
    }
}
