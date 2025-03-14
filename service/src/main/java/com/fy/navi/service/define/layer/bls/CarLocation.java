package com.fy.navi.service.define.layer.bls;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/12
 */
public class CarLocation {
    public int speed;
    public long un32Time;
    public ArrayList<PathMatchInfos> vecPathMatchInfo;

    public CarLocation() {
        vecPathMatchInfo = new ArrayList<>();
    }

    public int getSpeed() {
        return speed;
    }

    public void setSpeed(int speed) {
        this.speed = speed;
    }

    public long getUn32Time() {
        return un32Time;
    }

    public void setUn32Time(long un32Time) {
        this.un32Time = un32Time;
    }

    public ArrayList<PathMatchInfos> getVecPathMatchInfo() {
        return vecPathMatchInfo;
    }

    public void setVecPathMatchInfo(ArrayList<PathMatchInfos> vecPathMatchInfo) {
        this.vecPathMatchInfo = vecPathMatchInfo;
    }

    @Override
    public String toString() {
        return "CarLocation{" +
                "speed=" + speed +
                ", un32Time=" + un32Time +
                ", vecPathMatchInfo=" + vecPathMatchInfo +
                '}';
    }

    public static class PathMatchInfos{
        public double longitude;
        public double latitude;
        public float carDir;

        public PathMatchInfos() {
        }

        public PathMatchInfos(double longitude, double latitude, float carDir) {
            this.longitude = longitude;
            this.latitude = latitude;
            this.carDir = carDir;
        }

        public double getLongitude() {
            return longitude;
        }

        public void setLongitude(double longitude) {
            this.longitude = longitude;
        }

        public double getLatitude() {
            return latitude;
        }

        public void setLatitude(double latitude) {
            this.latitude = latitude;
        }

        public float getCarDir() {
            return carDir;
        }

        public void setCarDir(float carDir) {
            this.carDir = carDir;
        }

        @Override
        public String toString() {
            return "PathMatchInfos{" +
                    "longitude=" + longitude +
                    ", latitude=" + latitude +
                    ", carDir=" + carDir +
                    '}';
        }
    }
}
