package com.fy.navi.service.define.user.usertrack;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class FootprintNaviRecordInfo {
    public boolean arrived;
    public int monthActTimes;
    public int monthDistance;
    public String month;
    public String origin;
    public String destination;
    public String actualDest;
    public String naviId;

    public int duration;
    public int distance;
    public int avgSpeed;
    public int maxSpeed;
    public String avgSpeedDesc;
    public String avgSpeedUnit;
    public String maxSpeedDesc;
    public String maxSpeedUnit;

    public int maxCount;
    public int minNaviDist;
    public int maxNaviDist;
    public String naviTime;
    public String srcAdcode;
    public String dstAdcode;

    public int getMaxCount() {
        return maxCount;
    }

    public void setMaxCount(int maxCount) {
        this.maxCount = maxCount;
    }

    public int getMinNaviDist() {
        return minNaviDist;
    }

    public void setMinNaviDist(int minNaviDist) {
        this.minNaviDist = minNaviDist;
    }

    public int getMaxNaviDist() {
        return maxNaviDist;
    }

    public void setMaxNaviDist(int maxNaviDist) {
        this.maxNaviDist = maxNaviDist;
    }

    public String getNaviTime() {
        return naviTime;
    }

    public void setNaviTime(String naviTime) {
        this.naviTime = naviTime;
    }

    public String getSrcAdcode() {
        return srcAdcode;
    }

    public void setSrcAdcode(String srcAdcode) {
        this.srcAdcode = srcAdcode;
    }

    public String getDstAdcode() {
        return dstAdcode;
    }

    public void setDstAdcode(String dstAdcode) {
        this.dstAdcode = dstAdcode;
    }

    public boolean isArrived() {
        return arrived;
    }

    public void setArrived(boolean arrived) {
        this.arrived = arrived;
    }

    public int getMonthActTimes() {
        return monthActTimes;
    }

    public void setMonthActTimes(int monthActTimes) {
        this.monthActTimes = monthActTimes;
    }

    public int getMonthDistance() {
        return monthDistance;
    }

    public void setMonthDistance(int monthDistance) {
        this.monthDistance = monthDistance;
    }

    public String getMonth() {
        return month;
    }

    public void setMonth(String month) {
        this.month = month;
    }

    public String getOrigin() {
        return origin;
    }

    public void setOrigin(String origin) {
        this.origin = origin;
    }

    public String getDestination() {
        return destination;
    }

    public void setDestination(String destination) {
        this.destination = destination;
    }

    public String getActualDest() {
        return actualDest;
    }

    public void setActualDest(String actualDest) {
        this.actualDest = actualDest;
    }

    public String getNaviId() {
        return naviId;
    }

    public void setNaviId(String naviId) {
        this.naviId = naviId;
    }


    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public int getAvgSpeed() {
        return avgSpeed;
    }

    public void setAvgSpeed(int avgSpeed) {
        this.avgSpeed = avgSpeed;
    }

    public int getMaxSpeed() {
        return maxSpeed;
    }

    public void setMaxSpeed(int maxSpeed) {
        this.maxSpeed = maxSpeed;
    }

    public String getAvgSpeedDesc() {
        return avgSpeedDesc;
    }

    public void setAvgSpeedDesc(String avgSpeedDesc) {
        this.avgSpeedDesc = avgSpeedDesc;
    }

    public String getAvgSpeedUnit() {
        return avgSpeedUnit;
    }

    public void setAvgSpeedUnit(String avgSpeedUnit) {
        this.avgSpeedUnit = avgSpeedUnit;
    }

    public String getMaxSpeedDesc() {
        return maxSpeedDesc;
    }

    public void setMaxSpeedDesc(String maxSpeedDesc) {
        this.maxSpeedDesc = maxSpeedDesc;
    }

    public String getMaxSpeedUnit() {
        return maxSpeedUnit;
    }

    public void setMaxSpeedUnit(String maxSpeedUnit) {
        this.maxSpeedUnit = maxSpeedUnit;
    }
}
