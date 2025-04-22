package com.fy.navi.fsa.bean;

import java.util.Objects;

/**
 * 矢量放大图信息
 */
public class JunctionViewInfo {
    /**
     * remainDistance	int	剩余距离，单位：米
     */
    private int remainDistance;
    /**
     * progressRatio	int	进度百分比
     */
    private int progressRatio;
    /**
     * nextRoadName	String	放大图所在路名
     */
    private String nextRoadName;
    /**
     * turnKind	int	转向类型
     */
    private int turnKind;
    /**
     * imageBytes	String	放大图，Base64字符串，png/jpg
     */
    private String imageBytes;
    /**
     * imageMimeType	String	图类型。“image/png”或“image/jpeg”
     */
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
