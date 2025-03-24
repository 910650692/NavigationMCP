package com.fy.navi.service.define.user.usertrack;


public class GpsTrackPointBean {

    private double mF64Longitude;
    private double mF64Latitude;
    private double mF64Altitude;
    private float mF32Accuracy;
    private float mF32Speed;
    private float mF32Course;
    private long mN64TickTime;
    private int mN32SateliteTotal;
    private int mNSectionId;

    public double getF64Longitude() {
        return mF64Longitude;
    }

    public void setF64Longitude(final double f64Longitude) {
        this.mF64Longitude = f64Longitude;
    }

    public double getF64Latitude() {
        return mF64Latitude;
    }

    public void setF64Latitude(final double f64Latitude) {
        this.mF64Latitude = f64Latitude;
    }

    public double getF64Altitude() {
        return mF64Altitude;
    }

    public void setF64Altitude(final double f64Altitude) {
        this.mF64Altitude = f64Altitude;
    }

    public float getF32Accuracy() {
        return mF32Accuracy;
    }

    public void setF32Accuracy(final float f32Accuracy) {
        this.mF32Accuracy = f32Accuracy;
    }

    public float getF32Speed() {
        return mF32Speed;
    }

    public void setF32Speed(final float f32Speed) {
        this.mF32Speed = f32Speed;
    }

    public float getF32Course() {
        return mF32Course;
    }

    public void setF32Course(final float f32Course) {
        this.mF32Course = f32Course;
    }

    public long getN64TickTime() {
        return mN64TickTime;
    }

    public void setN64TickTime(final long n64TickTime) {
        this.mN64TickTime = n64TickTime;
    }

    public int getN32SateliteTotal() {
        return mN32SateliteTotal;
    }

    public void setN32SateliteTotal(final int n32SateliteTotal) {
        this.mN32SateliteTotal = n32SateliteTotal;
    }

    public int getSectionId() {
        return mNSectionId;
    }

    public void setSectionId(final int sectionId) {
        this.mNSectionId = sectionId;
    }
}
