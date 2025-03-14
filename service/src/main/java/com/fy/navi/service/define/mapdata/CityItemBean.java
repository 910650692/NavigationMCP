package com.fy.navi.service.define.mapdata;

import java.io.Serializable;

/**
 * @Description
 * @Author fh
 * @date 2024/12/11
 */
public class CityItemBean implements Serializable {
    //省adcode
    public int belongedProvince;
    // 城市经度
    public double cityX;
    // 城市纬度
    public double cityY;
    // 城市名称
    public String cityName;
    // 城市级别
    public int cityLevel;
    // 城市adcode
    public int cityAdcode;
    // 简拼
    public String initial;
    // 拼音
    public String pinyin;
    // 城市数据包下载状态信息
    public CityDownLoadInfo downLoadBean;

    public int getBelongedProvince() {
        return belongedProvince;
    }

    public void setBelongedProvince(int belongedProvince) {
        this.belongedProvince = belongedProvince;
    }

    public double getCityX() {
        return cityX;
    }

    public void setCityX(double cityX) {
        this.cityX = cityX;
    }

    public double getCityY() {
        return cityY;
    }

    public void setCityY(double cityY) {
        this.cityY = cityY;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public int getCityLevel() {
        return cityLevel;
    }

    public void setCityLevel(int cityLevel) {
        this.cityLevel = cityLevel;
    }

    public int getCityAdcode() {
        return cityAdcode;
    }

    public void setCityAdcode(int cityAdcode) {
        this.cityAdcode = cityAdcode;
    }

    public String getInitial() {
        return initial;
    }

    public void setInitial(String initial) {
        this.initial = initial;
    }

    public String getPinyin() {
        return pinyin;
    }

    public void setPinyin(String pinyin) {
        this.pinyin = pinyin;
    }
}
