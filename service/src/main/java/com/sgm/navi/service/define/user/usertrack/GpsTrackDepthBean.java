package com.sgm.navi.service.define.user.usertrack;

import java.util.ArrayList;


public class GpsTrackDepthBean {

    private String mFilePath;
    private String mFileName;
    private int mFastestIndex;
    private ArrayList<GpsTrackPointBean> mTrackPoints;
    private long mDuration;
    private long mDistance;
    private float mAverageSpeed;

    public String getFilePath() {
        return mFilePath;
    }

    public void setFilePath(final String filePath) {
        this.mFilePath = filePath;
    }

    public String getFileName() {
        return mFileName;
    }

    public void setFileName(final String fileName) {
        this.mFileName = fileName;
    }

    public int getFastestIndex() {
        return mFastestIndex;
    }

    public void setFastestIndex(final int fastestIndex) {
        this.mFastestIndex = fastestIndex;
    }

    public ArrayList<GpsTrackPointBean> getTrackPoints() {
        return mTrackPoints;
    }

    public void setTrackPoints(final ArrayList<GpsTrackPointBean> trackPoints) {
        this.mTrackPoints = trackPoints;
    }

    public long getDuration() {
        return mDuration;
    }

    public void setDuration(final long duration) {
        this.mDuration = duration;
    }

    public long getDistance() {
        return mDistance;
    }

    public void setDistance(final long distance) {
        this.mDistance = distance;
    }

    public float getAverageSpeed() {
        return mAverageSpeed;
    }

    public void setAverageSpeed(final float averageSpeed) {
        this.mAverageSpeed = averageSpeed;
    }
}
