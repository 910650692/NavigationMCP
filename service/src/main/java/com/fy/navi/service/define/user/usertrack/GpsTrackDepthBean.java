package com.fy.navi.service.define.user.usertrack;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class GpsTrackDepthBean {
    public String filePath;
    public String fileName;
    public int fastestIndex;
    public ArrayList<GpsTrackPointBean> trackPoints;
    public long duration;
    public long distance;
    public float averageSpeed;

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public int getFastestIndex() {
        return fastestIndex;
    }

    public void setFastestIndex(int fastestIndex) {
        this.fastestIndex = fastestIndex;
    }

    public ArrayList<GpsTrackPointBean> getTrackPoints() {
        return trackPoints;
    }

    public void setTrackPoints(ArrayList<GpsTrackPointBean> trackPoints) {
        this.trackPoints = trackPoints;
    }

    public long getDuration() {
        return duration;
    }

    public void setDuration(long duration) {
        this.duration = duration;
    }

    public long getDistance() {
        return distance;
    }

    public void setDistance(long distance) {
        this.distance = distance;
    }

    public float getAverageSpeed() {
        return averageSpeed;
    }

    public void setAverageSpeed(float averageSpeed) {
        this.averageSpeed = averageSpeed;
    }
}
