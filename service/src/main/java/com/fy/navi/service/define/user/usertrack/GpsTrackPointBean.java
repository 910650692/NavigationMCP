package com.fy.navi.service.define.user.usertrack;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class GpsTrackPointBean {
    public double f64Longitude;
    public double f64Latitude;
    public double f64Altitude;
    public float f32Accuracy;
    public float f32Speed;
    public float f32Course;
    public long n64TickTime;
    public int n32SateliteTotal;
    public int nSectionId;

    public double getF64Longitude() {
        return f64Longitude;
    }

    public void setF64Longitude(double f64Longitude) {
        this.f64Longitude = f64Longitude;
    }

    public double getF64Latitude() {
        return f64Latitude;
    }

    public void setF64Latitude(double f64Latitude) {
        this.f64Latitude = f64Latitude;
    }

    public double getF64Altitude() {
        return f64Altitude;
    }

    public void setF64Altitude(double f64Altitude) {
        this.f64Altitude = f64Altitude;
    }

    public float getF32Accuracy() {
        return f32Accuracy;
    }

    public void setF32Accuracy(float f32Accuracy) {
        this.f32Accuracy = f32Accuracy;
    }

    public float getF32Speed() {
        return f32Speed;
    }

    public void setF32Speed(float f32Speed) {
        this.f32Speed = f32Speed;
    }

    public float getF32Course() {
        return f32Course;
    }

    public void setF32Course(float f32Course) {
        this.f32Course = f32Course;
    }

    public long getN64TickTime() {
        return n64TickTime;
    }

    public void setN64TickTime(long n64TickTime) {
        this.n64TickTime = n64TickTime;
    }

    public int getN32SateliteTotal() {
        return n32SateliteTotal;
    }

    public void setN32SateliteTotal(int n32SateliteTotal) {
        this.n32SateliteTotal = n32SateliteTotal;
    }

    public int getnSectionId() {
        return nSectionId;
    }

    public void setnSectionId(int nSectionId) {
        this.nSectionId = nSectionId;
    }
}
