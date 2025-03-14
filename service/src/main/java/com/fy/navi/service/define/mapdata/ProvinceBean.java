package com.fy.navi.service.define.mapdata;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/11
 */
public class ProvinceBean {

    public String provName;
    public int provLevel;
    public int provAdcode;
    public double provX;
    public double provY;
    public String provInitial;
    public String provPinyin;
    public ArrayList<CityItemBean> cityInfoList;

    public String getProvName() {
        return provName;
    }

    public void setProvName(String provName) {
        this.provName = provName;
    }

    public int getProvLevel() {
        return provLevel;
    }

    public void setProvLevel(int provLevel) {
        this.provLevel = provLevel;
    }

    public int getProvAdcode() {
        return provAdcode;
    }

    public void setProvAdcode(int provAdcode) {
        this.provAdcode = provAdcode;
    }

    public double getProvX() {
        return provX;
    }

    public void setProvX(double provX) {
        this.provX = provX;
    }

    public double getProvY() {
        return provY;
    }

    public void setProvY(double provY) {
        this.provY = provY;
    }

    public String getProvInitial() {
        return provInitial;
    }

    public void setProvInitial(String provInitial) {
        this.provInitial = provInitial;
    }

    public String getProvPinyin() {
        return provPinyin;
    }

    public void setProvPinyin(String provPinyin) {
        this.provPinyin = provPinyin;
    }

    public ArrayList<CityItemBean> getCityInfoList() {
        return cityInfoList;
    }

    public void setCityInfoList(ArrayList<CityItemBean> cityInfoList) {
        this.cityInfoList = cityInfoList;
    }
}
