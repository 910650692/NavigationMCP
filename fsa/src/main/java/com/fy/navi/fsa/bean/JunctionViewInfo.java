package com.fy.navi.fsa.bean;

import java.util.Objects;

public class JunctionViewInfo {
    private int remainDistance;
    private int progressRatio;
    private String nextRoadName;
    private int turnKind;
    private String imageBytes;
    private String imageMimeType;

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

    public String getNextRoadName() {
        return nextRoadName;
    }

    public void setNextRoadName(String nextRoadName) {
        this.nextRoadName = nextRoadName;
    }

    public int getTurnKind() {
        return turnKind;
    }

    public void setTurnKind(int turnKind) {
        this.turnKind = turnKind;
    }

    public String getImageBytes() {
        return imageBytes;
    }

    public void setImageBytes(String imageBytes) {
        this.imageBytes = imageBytes;
    }

    public String getImageMimeType() {
        return imageMimeType;
    }

    public void setImageMimeType(String imageMimeType) {
        this.imageMimeType = imageMimeType;
    }

    @Override
    public String toString() {
        return "JunctionViewInfo{" +
                "remainDistance=" + remainDistance +
                ", progressRatio=" + progressRatio +
                ", nextRoadName='" + nextRoadName + '\'' +
                ", turnKind=" + turnKind +
                ", imageBytes='" + (Objects.isNull(imageBytes) ? "null" : imageBytes.length()) + '\'' +
                ", imageMimeType='" + imageMimeType + '\'' +
                '}';
    }
}
